/*
 * PicoDrive
 * (c) Copyright Dave, 2004
 * (C) notaz, 2006-2009
 *
 * This work is licensed under the terms of MAME license.
 * See COPYING file in the top-level directory.
 */

#include "pico_int.h"
#define NEED_DMA_SOURCE
#include "memory.h"

extern const unsigned char  hcounts_32[];
extern const unsigned char  hcounts_40[];

static int blankline;           // display disabled for this line

int (*PicoDmaHook)(unsigned int source, int len, unsigned short **base, unsigned int *mask) = NULL;


/* VDP FIFO implementation
 * 
 * fifo_slot: last slot executed in this scanline
 * fifo_cnt: #slots remaining for active FIFO write (#writes<<#bytep)
 * fifo_total: #total FIFO entries pending
 * fifo_data: last values transferred through fifo
 * fifo_queue: fifo transfer queue (#writes, flags)
 *
 * FIFO states:		empty	total=0
 *			inuse	total>0 && total<4
 *			full	total==4
 *			wait	total>4
 * Conditions:
 * fifo_slot is always behind slot2cyc[cycles]. Advancing it beyond cycles
 * implies blocking the 68k up to that slot.
 *
 * A FIFO write goes to the end of the fifo queue. There can be more pending
 * writes than FIFO slots, but the 68k will be blocked in most of those cases.
 * This is only about correct timing, data xfer must be handled by the caller.
 * Blocking the CPU means burning cycles via SekCyclesBurn*(), which is to be
 * executed by the caller.
 *
 * FIFOSync "executes" FIFO write slots up to the given cycle in the current
 * scanline. A queue entry completely executed is removed from the queue.
 * FIFOWrite pushes writes to the transfer queue. If it's a blocking write, 68k
 * is blocked if more than 4 FIFO writes are pending.
 * FIFORead executes a 68k read. 68k is blocked until the next transfer slot.
 */

// FIFO transfer slots per line: H32 blank, H40 blank, H32 active, H40 active
static const short vdpslots[] = { 166, 204, 16, 18 };
// mapping between slot# and 68k cycles in a blanked scanline
static const int vdpcyc2sl_bl[] = { (166<<16)/488, (204<<16)/488, (16<<16)/488, (18<<16)/488 };
static const int vdpsl2cyc_bl[] = { (488<<16)/166, (488<<16)/204, (488<<16)/16, (488<<16)/18 };

// VDP transfer slots in active display 32col mode. 1 slot is 488/171 = 2.8538
// 68k cycles. Only 16 of the 171 slots in a scanline can be used by CPU/DMA:
// (HINT=slot 0): 13,27,42,50,58,74,82,90,106,114,122,138,146,154,169,170
const unsigned char vdpcyc2sl_32[] = { // 68k cycles/4 since HINT to slot #
//  4  8 12 16 20 24 28 32 36 40 44 48 52 56 60
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3,
 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5,
 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9,
 9,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,
11,11,12,12,12,12,12,12,13,13,13,13,13,13,14,14,
14,14,14,14,14,14,14,14,15,16,16,16,16,16,16,16,
};
const unsigned char vdpsl2cyc_32[] = { // slot # to 68k cycles/4 since HINT
  0,  9, 19, 30, 35, 41, 52, 58, 64, 75, 81, 87, 98,104,110,120,121,123
};

// VDP transfer slots in active display 40col mode. 1 slot is 488/210 = 2.3238
// 68k cycles. Only 18 of the 210 slots in a scanline can be used by CPU/DMA:
// (HINT=0): 23,49,57,65,81,89,97,113,121,129,145,153,161,177,185,193,208,209
const unsigned char vdpcyc2sl_40[] = { // 68k cycles/4 since HINT to slot #
//  4  8 12 16 20 24 28 32 36 40 44 48 52 56 60
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5,
 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 8, 8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,10,10,
10,10,10,10,11,11,11,11,12,12,12,12,12,13,13,13,
13,13,13,13,13,13,14,14,14,14,14,15,15,15,15,15,
16,16,16,16,16,16,16,16,17,18,18,18,18,18,18,18,
};
const unsigned char vdpsl2cyc_40[] = { // slot # to 68k cycles/4 since HINT
  0, 13, 28, 33, 37, 47, 51, 56, 65, 70, 74, 84, 88, 93,102,107,112,120,121,123
};

// NB code assumes fifo_* arrays have size 2^n
// last transferred FIFO data, ...x = index  XXX currently only CPU
static short fifo_data[4], fifo_dx; // XXX must go into save?

// queued FIFO transfers, ...x = index, ...l = queue length
// each entry has 2 values: [n]>>2=#writes, [n]&3=flags:2=DMA fill 1=byte access
static int fifo_queue[8], fifo_qx, fifo_ql; // XXX must go into save?
unsigned int fifo_total;        // total# of pending FIFO entries

unsigned short fifo_slot;       // last executed slot in current scanline

// do the FIFO math
static __inline int AdvanceFIFOEntry(struct PicoVideo *pv, int slots)
{
  int l = slots, b = fifo_queue[fifo_qx&7] & 1;

  if (l > pv->fifo_cnt)
    l = pv->fifo_cnt;
  fifo_total -= ((pv->fifo_cnt & b) + l) >> b;
  pv->fifo_cnt -= l;

  if (pv->fifo_cnt == 0) {
    if (fifo_ql)
      fifo_qx ++, fifo_ql --;
    if (fifo_ql)
      pv->fifo_cnt= (fifo_queue[fifo_qx&7] >> 2) << (fifo_queue[fifo_qx&7] & 1);
    else
      fifo_total = 0;
  }
  return l;
}

static __inline int GetFIFOSlot(struct PicoVideo *pv, int cycles)
{
  int active = !(pv->status & SR_VB) && (pv->reg[1] & 0x40);
  int h40 = pv->reg[12] & 1;
  const unsigned char *cs = h40 ? vdpcyc2sl_40 : vdpcyc2sl_32;

  if (active)	return cs[cycles/4];
  else		return (cycles * vdpcyc2sl_bl[h40] + cycles) >> 16;
}

static __inline int GetFIFOCycles(struct PicoVideo *pv, int slot)
{
  int active = !(pv->status & SR_VB) && (pv->reg[1] & 0x40);
  int h40 = pv->reg[12] & 1;
  const unsigned char *sc = h40 ? vdpsl2cyc_40 : vdpsl2cyc_32;

  if (active)	return sc[slot]*4;
  else		return ((slot * vdpsl2cyc_bl[h40] + slot) >> 16);
}

// sync FIFO to cycles
void PicoVideoFIFOSync(int cycles)
{
  struct PicoVideo *pv = &Pico.video;
  int slots, done;

  // calculate #slots since last executed slot
  slots = GetFIFOSlot(pv, cycles) - fifo_slot;

  // advance FIFO queue by #done slots
  done = slots;
  while (done > 0 && pv->fifo_cnt) {
    int l = AdvanceFIFOEntry(pv, done);
    fifo_slot += l;
    done -= l;
  }

  // release CPU and terminate DMA if FIFO isn't blocking the 68k anymore
  if (fifo_total <= 4) {
    pv->status &= ~PVS_CPUWR;
    pv->command &= ~0x80;
    if (!(pv->status & PVS_DMAPEND))
      pv->status &= ~(SR_DMA|PVS_DMAFILL);
  }
  if (fifo_total == 0)
    pv->status &= ~PVS_CPURD;
}

// drain FIFO, blocking 68k on the way. FIFO must be synced prior to drain.
int PicoVideoFIFODrain(int level, int cycles)
{
  struct PicoVideo *pv = &Pico.video;
  int active = !(pv->status & SR_VB) && (pv->reg[1] & 0x40);
  int h40 = pv->reg[12] & 1;
  int maxsl = vdpslots[h40 + 2*active]; // max xfer slots in this scanline
  int burn = 0;

  while (fifo_total > level && fifo_slot < maxsl) {
    int b = fifo_queue[fifo_qx&7] & 1;
    int cnt = (fifo_total-level) << b;
    int last = fifo_slot;
    int slot = (pv->fifo_cnt<cnt?pv->fifo_cnt:cnt) + last; // target slot
    unsigned ocyc = cycles;

    if (slot > maxsl) {
      // target in later scanline, advance to eol
      slot = maxsl;
      fifo_slot = maxsl;
      cycles = 488;
    } else {
      // advance FIFO to target slot and CPU to cycles at that slot
      fifo_slot = slot;
      cycles = GetFIFOCycles(pv, slot);
    }
    burn += cycles - ocyc;

    AdvanceFIFOEntry(pv, slot - last);
  }

  // release CPU and terminate DMA if FIFO isn't blocking the bus anymore
  if (fifo_total <= 4) {
    pv->status &= ~PVS_CPUWR;
    pv->command &= ~0x80;
    if (!(pv->status & PVS_DMAPEND))
      pv->status &= ~(SR_DMA|PVS_DMAFILL);
  }
  if (fifo_total == 0)
    pv->status &= ~PVS_CPURD;

  return burn;
}

// read VDP data port
int PicoVideoFIFORead(void)
{
  struct PicoVideo *pv = &Pico.video;
  int lc = SekCyclesDone()-Pico.t.m68c_line_start+4;
  int burn = 0;

  PicoVideoFIFOSync(lc);

  // advance FIFO and CPU until FIFO is empty
  burn = PicoVideoFIFODrain(0, lc);
  lc += burn;
  if (fifo_total > 0)
    pv->status |= PVS_CPURD; // target slot is in later scanline
  else {
    // use next VDP access slot for reading, block 68k until then
    fifo_slot = GetFIFOSlot(pv, lc) + 1;
    burn += GetFIFOCycles(pv, fifo_slot) - lc;
  }

  return burn;
}
 
// write VDP data port
int PicoVideoFIFOWrite(int count, int flags, unsigned sr_mask,unsigned sr_flags)
{
  struct PicoVideo *pv = &Pico.video;
  int lc = SekCyclesDone()-Pico.t.m68c_line_start+4;
  int burn = 0;

  PicoVideoFIFOSync(lc);
  pv->status = (pv->status & ~sr_mask) | sr_flags;

  if (count && fifo_ql < 8) {
    // update FIFO state if it was empty
    if (fifo_total == 0 && count) {
      fifo_slot = GetFIFOSlot(pv, lc);
      pv->fifo_cnt = count << (flags&1);
    }

    // create xfer queue entry
    int x = (fifo_qx + fifo_ql) & 7;
    fifo_queue[x] = (count << 2) | flags;
    fifo_ql ++;
    fifo_total += count;
  }

  // if CPU is waiting for the bus, advance CPU and FIFO until bus is free
  if ((pv->status & (PVS_CPUWR|PVS_DMAFILL)) == PVS_CPUWR)
    burn = PicoVideoFIFODrain(4, lc);
  else if (fifo_queue[fifo_qx&7]&2) {
    // if interrupting a DMA fill terminate it XXX wrong, changes fill data
    AdvanceFIFOEntry(pv, pv->fifo_cnt);
    pv->status &= ~PVS_DMAFILL;
  }

  return burn;
}

// at HINT, advance FIFO to new scanline
int PicoVideoFIFOHint(void)
{
  struct PicoVideo *pv = &Pico.video;
  int burn = 0;

  // reset slot to start of scanline
  fifo_slot = 0;
 
  // if CPU is waiting for the bus, advance CPU and FIFO until bus is free
  if (pv->status & PVS_CPURD)
    burn = PicoVideoFIFORead();
  if (pv->status & PVS_CPUWR)
    burn = PicoVideoFIFOWrite(0, 0, 0, 0);

  return burn;
}

// switch FIFO mode between active/inactive display
void PicoVideoFIFOMode(int active)
{
  struct PicoVideo *pv = &Pico.video;
  const unsigned char *cs = pv->reg[12]&1 ? vdpcyc2sl_40 : vdpcyc2sl_32;
  int h40 = pv->reg[12] & 1;
  int lc = SekCyclesDone() - Pico.t.m68c_line_start;

  PicoVideoFIFOSync(lc);

  if (fifo_total) {
    // recalculate FIFO slot for new mode
    if (!(pv->status & SR_VB) && active)
          fifo_slot = cs[lc/4];
    else  fifo_slot = ((lc * vdpcyc2sl_bl[h40] + lc) >> 16);
  }
}


// VDP memory rd/wr

static __inline void AutoIncrement(void)
{
  Pico.video.addr=(unsigned short)(Pico.video.addr+Pico.video.reg[0xf]);
}

static NOINLINE unsigned int VideoWrite128(u32 a, u16 d)
{
  // nasty
  a = ((a & 2) >> 1) | ((a & 0x400) >> 9) | (a & 0x3FC) | ((a & 0x1F800) >> 1);
  ((u8 *)PicoMem.vram)[a] = d;
  return a;
}

static void VideoWrite(u16 d)
{
  unsigned int a = Pico.video.addr;

  switch (Pico.video.type)
  {
    case 1: if (a & 1)
              d = (u16)((d << 8) | (d >> 8));
            PicoMem.vram [(a >> 1) & 0x7fff] = d;
            if ((unsigned)(a - ((Pico.video.reg[5]&0x7f) << 9)) < 0x400)
              Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
            break;
    case 3: if (PicoMem.cram [(a >> 1) & 0x3f] != d) Pico.m.dirtyPal = 1;
            PicoMem.cram [(a >> 1) & 0x3f] = d; break;
    case 5: PicoMem.vsram[(a >> 1) & 0x3f] = d; break;
    case 0x81: if (a & 1)
              d = (u16)((d << 8) | (d >> 8));
            a |= Pico.video.addr_u << 16;
            a = VideoWrite128(a, d);
            if ((unsigned)(a - ((Pico.video.reg[5]&0x7f) << 9)) < 0x400)
              Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
            break;
    //default:elprintf(EL_ANOMALY, "VDP write %04x with bad type %i", d, Pico.video.type); break;
  }

  AutoIncrement();
}

static unsigned int VideoRead(void)
{
  unsigned int a, d = fifo_data[(fifo_dx+1)&3];

  a=Pico.video.addr; a>>=1;

  SekCyclesBurnRun(PicoVideoFIFORead());
  switch (Pico.video.type)
  {
    case 0: d=PicoMem.vram [a & 0x7fff]; break;
    case 8: d=(PicoMem.cram [a & 0x003f] & 0x0eee) | (d & ~0x0eee); break;
    case 4: if ((a & 0x3f) >= 0x28) a = 0;
            d=(PicoMem.vsram [a & 0x003f] & 0x07ff) | (d & ~0x07ff); break;
    case 12:a=PicoMem.vram [a & 0x7fff]; if (Pico.video.addr&1) a >>= 8;
            d=(a & 0x00ff) | (d & ~0x00ff); break;
    default:elprintf(EL_ANOMALY, "VDP read with bad type %i", Pico.video.type); break;
  }

  AutoIncrement();
  return d;
}

// VDP DMA

static int GetDmaLength(void)
{
  struct PicoVideo *pvid=&Pico.video;
  int len=0;
  // 16-bit words to transfer:
  len =pvid->reg[0x13];
  len|=pvid->reg[0x14]<<8;
  len = ((len - 1) & 0xffff) + 1;
  return len;
}

static void DmaSlow(int len, unsigned int source)
{
  u32 inc = Pico.video.reg[0xf];
  u32 a = Pico.video.addr;
  u16 *r, *base = NULL;
  u32 mask = 0x1ffff;

  elprintf(EL_VDPDMA, "DmaSlow[%i] %06x->%04x len %i inc=%i blank %i [%u] @ %06x",
    Pico.video.type, source, a, len, inc, (Pico.video.status&SR_VB)||!(Pico.video.reg[1]&0x40),
    SekCyclesDone(), SekPc);

  SekCyclesBurnRun(PicoVideoFIFOWrite(len, Pico.video.type == 1, PVS_DMAPEND,
                                        SR_DMA | PVS_CPUWR) + 8);

  if ((source & 0xe00000) == 0xe00000) { // Ram
    base = (u16 *)PicoMem.ram;
    mask = 0xffff;
  }
  else if (PicoIn.AHW & PAHW_MCD)
  {
    u8 r3 = Pico_mcd->s68k_regs[3];
    elprintf(EL_VDPDMA, "DmaSlow CD, r3=%02x", r3);
    if (source < 0x20000) { // Bios area
      base = (u16 *)Pico_mcd->bios;
    } else if ((source & 0xfc0000) == 0x200000) { // Word Ram
      if (!(r3 & 4)) { // 2M mode
        base = (u16 *)(Pico_mcd->word_ram2M + (source & 0x20000));
      } else {
        if (source < 0x220000) { // 1M mode
          int bank = r3 & 1;
          base = (u16 *)(Pico_mcd->word_ram1M[bank]);
        } else {
          DmaSlowCell(source - 2, a, len, inc);
          return;
        }
      }
      source -= 2;
    } else if ((source & 0xfe0000) == 0x020000) { // Prg Ram
      base = (u16 *)Pico_mcd->prg_ram_b[r3 >> 6];
      source -= 2; // XXX: test
    }
  }
  else
  {
    // if we have DmaHook, let it handle ROM because of possible DMA delay
    u32 source2;
    if (PicoDmaHook && (source2 = PicoDmaHook(source, len, &base, &mask)))
      source = source2;
    else // Rom
      base = m68k_dma_source(source);
  }
  if (!base) {
    elprintf(EL_VDPDMA|EL_ANOMALY, "DmaSlow[%i] %06x->%04x: invalid src", Pico.video.type, source, a);
    return;
  }

  // operate in words
  source >>= 1;
  mask >>= 1;

  switch (Pico.video.type)
  {
    case 1: // vram
      r = PicoMem.vram;
      if (inc == 2 && !(a & 1) && a + len * 2 < 0x10000
          && !(((source + len - 1) ^ source) & ~mask))
      {
        // most used DMA mode
        memcpy((char *)r + a, base + (source & mask), len * 2);
        a += len * 2;
      }
      else
      {
        for(; len; len--)
        {
          u16 d = base[source++ & mask];
          if(a & 1) d=(d<<8)|(d>>8);
          r[a >> 1] = d;
          // AutoIncrement
          a = (u16)(a + inc);
        }
      }
      Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
      break;

    case 3: // cram
      Pico.m.dirtyPal = 1;
      r = PicoMem.cram;
      for (; len; len--)
      {
        r[(a / 2) & 0x3f] = base[source++ & mask];
        // AutoIncrement
        a += inc;
      }
      break;

    case 5: // vsram
      r = PicoMem.vsram;
      for (; len; len--)
      {
        r[(a / 2) & 0x3f] = base[source++ & mask];
        // AutoIncrement
        a += inc;
      }
      break;

    case 0x81: // vram 128k
      a |= Pico.video.addr_u << 16;
      for(; len; len--)
      {
        VideoWrite128(a, base[source++ & mask]);
        // AutoIncrement
        a = (a + inc) & 0x1ffff;
      }
      Pico.video.addr_u = a >> 16;
      Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
      break;

    default:
      if (Pico.video.type != 0 || (EL_LOGMASK & EL_VDPDMA))
        elprintf(EL_VDPDMA|EL_ANOMALY, "DMA with bad type %i", Pico.video.type);
      break;
  }
  // remember addr
  Pico.video.addr=(u16)a;
}

static void DmaCopy(int len)
{
  u16 a = Pico.video.addr;
  u8 *vr = (u8 *)PicoMem.vram;
  u8 inc = Pico.video.reg[0xf];
  int source;
  elprintf(EL_VDPDMA, "DmaCopy len %i [%u]", len, SekCyclesDone());

  SekCyclesBurnRun(PicoVideoFIFOWrite(len, 1, PVS_CPUWR | PVS_DMAPEND, SR_DMA));

  source =Pico.video.reg[0x15];
  source|=Pico.video.reg[0x16]<<8;

  // XXX implement VRAM 128k? Is this even working?
  for (; len; len--)
  {
    vr[a] = vr[source++ & 0xffff];
    // AutoIncrement
    a=(u16)(a+inc);
  }
  // remember addr
  Pico.video.addr=a;
  Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
}

static NOINLINE void DmaFill(int data)
{
  u16 a = Pico.video.addr;
  u8 *vr = (u8 *)PicoMem.vram;
  u8 high = (u8)(data >> 8);
  u8 inc = Pico.video.reg[0xf];
  int source;
  int len, l;

  len = GetDmaLength();
  elprintf(EL_VDPDMA, "DmaFill len %i inc %i [%u]", len, inc, SekCyclesDone());

  SekCyclesBurnRun(PicoVideoFIFOWrite(len, 2|(Pico.video.type == 1),
                                          PVS_CPUWR | PVS_DMAPEND, SR_DMA));

  switch (Pico.video.type)
  {
    case 1: // vram
      for (l = len; l; l--) {
        // Write upper byte to adjacent address
        // (here we are byteswapped, so address is already 'adjacent')
        vr[a] = high;

        // Increment address register
        a = (u16)(a + inc);
      }
      Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
      break;
    case 3:   // cram
      Pico.m.dirtyPal = 1;
      for (l = len; l; l--) {
        PicoMem.cram[(a/2) & 0x3f] = data;

        // Increment address register
        a += inc;
      }
      break;
    case 5: { // vsram
      for (l = len; l; l--) {
        PicoMem.vsram[(a/2) & 0x3f] = data;

        // Increment address register
        a += inc;
      }
      break;
    }
    case 0x81: // vram 128k
      for (l = len; l; l--) {
        VideoWrite128(a, data);

        // Increment address register
        a = (a + inc) & 0x1ffff;
      }
      Pico.video.addr_u = a >> 16;
      Pico.est.rendstatus |= PDRAW_DIRTY_SPRITES;
      break;
    default:
      a += len * inc;
      break;
  }

  // remember addr
  Pico.video.addr = a;
  // register update
  Pico.video.reg[0x13] = Pico.video.reg[0x14] = 0;
  source  = Pico.video.reg[0x15];
  source |= Pico.video.reg[0x16] << 8;
  source += len;
  Pico.video.reg[0x15] = source;
  Pico.video.reg[0x16] = source >> 8;

}

// VDP command handling

static NOINLINE void CommandDma(void)
{
  struct PicoVideo *pvid=&Pico.video;
  u32 len, method;
  u32 source;

  pvid->status |= PVS_DMAPEND;
  PicoVideoFIFOSync(SekCyclesDone()-Pico.t.m68c_line_start);
  if (pvid->status & SR_DMA) {
    elprintf(EL_VDPDMA, "Dma overlap, left=%d @ %06x",
             fifo_total, SekPc);
    fifo_total = fifo_ql = 0;
  }
  pvid->status |= SR_DMA;

  len = GetDmaLength();
  source =Pico.video.reg[0x15];
  source|=Pico.video.reg[0x16] << 8;
  source|=Pico.video.reg[0x17] << 16;

  method=pvid->reg[0x17]>>6;
  if (method < 2)
    DmaSlow(len, source << 1); // 68000 to VDP
  else if (method == 3)
    DmaCopy(len); // VRAM Copy
  else {
    pvid->status |= PVS_DMAFILL;
    return;
  }
  source += len;
  Pico.video.reg[0x13] = Pico.video.reg[0x14] = 0;
  Pico.video.reg[0x15] = source;
  Pico.video.reg[0x16] = source >> 8;
}

static NOINLINE void CommandChange(void)
{
  struct PicoVideo *pvid = &Pico.video;
  unsigned int cmd, addr;

  cmd = pvid->command;

  // Get type of transfer 0xc0000030 (v/c/vsram read/write)
  pvid->type = (u8)(((cmd >> 2) & 0xc) | (cmd >> 30));
  if (pvid->type == 1) // vram
    pvid->type |= pvid->reg[1] & 0x80; // 128k

  // Get address 0x3fff0003
  addr  = (cmd >> 16) & 0x3fff;
  addr |= (cmd << 14) & 0xc000;
  pvid->addr = (u16)addr;
  pvid->addr_u = (u8)((cmd >> 2) & 1);
}

// VDP interface
 
static void DrawSync(int skip)
{
  int lines = Pico.video.reg[1]&0x08 ? 240 : 224;
  int last = Pico.m.scanline - (skip || blankline == Pico.m.scanline);

  if (last < lines && !(PicoIn.opt & POPT_ALT_RENDERER) &&
      !PicoIn.skipFrame && Pico.est.DrawScanline <= last) {
    //elprintf(EL_ANOMALY, "sync");
    if (blankline >= 0 && blankline < last) {
      PicoDrawSync(blankline, 1);
      blankline = -1;
    }
    PicoDrawSync(last, 0);
  }
}

PICO_INTERNAL_ASM void PicoVideoWrite(unsigned int a,unsigned short d)
{
  struct PicoVideo *pvid=&Pico.video;

  //elprintf(EL_STATUS, "PicoVideoWrite [%06x] %04x [%u] @ %06x",
  //  a, d, SekCyclesDone(), SekPc);

  a &= 0x1c;
  switch (a)
  {
  case 0x00: // Data port 0 or 2
    // try avoiding the sync..
    if (Pico.m.scanline < (pvid->reg[1]&0x08 ? 240 : 224) && (pvid->reg[1]&0x40) &&
        !(!pvid->pending &&
          ((pvid->command & 0xc00000f0) == 0x40000010 && PicoMem.vsram[pvid->addr>>1] == (d & 0x7ff)))
       )
      DrawSync(SekCyclesDone() - Pico.t.m68c_line_start <= 488-440);

    if (pvid->pending) {
      CommandChange();
      pvid->pending=0;
    }

    if (!(PicoIn.opt&POPT_DIS_VDP_FIFO))
    {
      fifo_data[++fifo_dx&3] = d;
      SekCyclesBurnRun(PicoVideoFIFOWrite(1, pvid->type == 1, 0, PVS_CPUWR));

      elprintf(EL_ASVDP, "VDP data write: [%04x] %04x [%u] {%i} @ %06x",
        Pico.video.addr, d, SekCyclesDone(), Pico.video.type, SekPc);
    }
    VideoWrite(d);

    // start DMA fill on write. NB VSRAM and CRAM fills use wrong FIFO data.
    if ((pvid->status & (PVS_DMAPEND|PVS_DMAFILL)) == (PVS_DMAPEND|PVS_DMAFILL))
      DmaFill(fifo_data[(fifo_dx + !!(pvid->type&~0x81))&3]);

    break;

  case 0x04: // Control (command) port 4 or 6
    if (pvid->pending)
    {
      // Low word of command:
      if (!(pvid->reg[1]&0x10))
        d = (d&~0x80)|(pvid->command&0x80);
      pvid->command &= 0xffff0000;
      pvid->command |= d;
      pvid->pending = 0;
      CommandChange();
      // Check for dma:
      if (d & 0x80) {
        DrawSync(SekCyclesDone() - Pico.t.m68c_line_start <= 488-390);
        CommandDma();
      }
    }
    else
    {
      if ((d&0xc000)==0x8000)
      {
        // Register write:
        int num=(d>>8)&0x1f;
        int dold=pvid->reg[num];
        pvid->type=0; // register writes clear command (else no Sega logo in Golden Axe II)
        if (num > 0x0a && !(pvid->reg[1]&4)) {
          elprintf(EL_ANOMALY, "%02x written to reg %02x in SMS mode @ %06x", d, num, SekPc);
          return;
        }

        if (num == 0 && !(pvid->reg[0]&2) && (d&2))
          pvid->hv_latch = PicoVideoRead(0x08);
        if (num == 1 && ((pvid->reg[1]^d)&0x40)) {
          PicoVideoFIFOMode(d & 0x40);
          // handle line blanking before line rendering
          if (SekCyclesDone() - Pico.t.m68c_line_start <= 488-390)
            blankline = d&0x40 ? -1 : Pico.m.scanline;
        }
        DrawSync(SekCyclesDone() - Pico.t.m68c_line_start <= 488-390);
        pvid->reg[num]=(unsigned char)d;
        switch (num)
        {
          case 0x00:
            elprintf(EL_INTSW, "hint_onoff: %i->%i [%u] pend=%i @ %06x", (dold&0x10)>>4,
                    (d&0x10)>>4, SekCyclesDone(), (pvid->pending_ints&0x10)>>4, SekPc);
            goto update_irq;
          case 0x01:
            elprintf(EL_INTSW, "vint_onoff: %i->%i [%u] pend=%i @ %06x", (dold&0x20)>>5,
                    (d&0x20)>>5, SekCyclesDone(), (pvid->pending_ints&0x20)>>5, SekPc);
            if (!(pvid->status & PVS_VB2))
              pvid->status &= ~SR_VB;
            pvid->status |= ((d >> 3) ^ SR_VB) & SR_VB; // forced blanking
            goto update_irq;
          case 0x05:
            //elprintf(EL_STATUS, "spritep moved to %04x", (unsigned)(Pico.video.reg[5]&0x7f) << 9);
            if (d^dold) Pico.est.rendstatus |= PDRAW_SPRITES_MOVED;
            break;
          case 0x0c:
            // renderers should update their palettes if sh/hi mode is changed
            if ((d^dold)&8) Pico.m.dirtyPal = 1;
            break;
        }
        return;

update_irq:
#ifndef EMU_CORE_DEBUG
        // update IRQ level
        if (!SekShouldInterrupt()) // hack
        {
          int lines, pints, irq = 0;
          lines = (pvid->reg[1] & 0x20) | (pvid->reg[0] & 0x10);
          pints = pvid->pending_ints & lines;
               if (pints & 0x20) irq = 6;
          else if (pints & 0x10) irq = 4;
          SekInterrupt(irq); // update line

          // this is broken because cost of current insn isn't known here
          if (irq) SekEndRun(21); // make it delayed
        }
#endif
      }
      else
      {
        // High word of command:
        pvid->command&=0x0000ffff;
        pvid->command|=d<<16;
        pvid->pending=1;
      }
    }
    break;

  // case 0x08: // 08 0a - HV counter - lock up
  // case 0x0c: // 0c 0e - HV counter - lock up
  // case 0x10: // 10 12 - PSG - handled by caller
  // case 0x14: // 14 16 - PSG - handled by caller
  // case 0x18: // 18 1a - no effect?
  case 0x1c: // 1c 1e - debug
    pvid->debug = d;
    pvid->debug_p = 0;
    if (d & (1 << 6)) {
      pvid->debug_p |= PVD_KILL_A | PVD_KILL_B;
      pvid->debug_p |= PVD_KILL_S_LO | PVD_KILL_S_HI;
    }
    switch ((d >> 7) & 3) {
      case 1:
        pvid->debug_p &= ~(PVD_KILL_S_LO | PVD_KILL_S_HI);
        pvid->debug_p |= PVD_FORCE_S;
        break;
      case 2:
        pvid->debug_p &= ~PVD_KILL_A;
        pvid->debug_p |= PVD_FORCE_A;
        break;
      case 3:
        pvid->debug_p &= ~PVD_KILL_B;
        pvid->debug_p |= PVD_FORCE_B;
        break;
    }
    break;
  }
}

static u32 VideoSr(const struct PicoVideo *pv)
{
  unsigned int c, d = pv->status;
  unsigned int hp = pv->reg[12]&1 ? 32:40; // HBLANK start
  unsigned int hl = pv->reg[12]&1 ? 94:84; // HBLANK length

  c = SekCyclesDone();
  if (c - Pico.t.m68c_line_start - hp < hl)
    d |= SR_HB;

  PicoVideoFIFOSync(c-Pico.t.m68c_line_start);
  if (pv->status & SR_DMA)
    d |= SR_EMPT;       // unused by DMA, or rather flags not updated?
  else if (fifo_total >= 4)
    d |= SR_FULL;
  else if (!fifo_total)
    d |= SR_EMPT;
  return d;
}

PICO_INTERNAL_ASM unsigned int PicoVideoRead(unsigned int a)
{
  a &= 0x1c;

  if (a == 0x04) // control port
  {
    struct PicoVideo *pv = &Pico.video;
    unsigned int d = VideoSr(pv);
    if (pv->pending) {
      CommandChange();
      pv->pending = 0;
    }
    elprintf(EL_SR, "SR read: %04x [%u] @ %06x", d, SekCyclesDone(), SekPc);
    return d;
  }

  // H-counter info (based on Generator):
  // frame:
  //                       |       <- hblank? ->      |
  // start    <416>       hint  <36> hdisplay <38>  end // CPU cycles
  // |---------...---------|------------|-------------|
  // 0                   B6 E4                       FF // 40 cells
  // 0                   93 E8                       FF // 32 cells

  // Gens (?)              v-render
  // start  <hblank=84>   hint    hdisplay <404>      |
  // |---------------------|--------------------------|
  // E4  (hc[0x43]==0)    07                         B1 // 40
  // E8  (hc[0x45]==0)    05                         91 // 32

  // check: Sonic 3D Blast bonus, Cannon Fodder, Chase HQ II, 3 Ninjas kick back, Road Rash 3, Skitchin', Wheel of Fortune
  if ((a&0x1c)==0x08)
  {
    unsigned int d;

    d = (SekCyclesDone() - Pico.t.m68c_line_start) & 0x1ff; // FIXME
    if (Pico.video.reg[0]&2)
         d = Pico.video.hv_latch;
    else if (Pico.video.reg[12]&1)
         d = hcounts_40[d] | (Pico.video.v_counter << 8);
    else d = hcounts_32[d] | (Pico.video.v_counter << 8);

    elprintf(EL_HVCNT, "hv: %02x %02x [%u] @ %06x", d, Pico.video.v_counter, SekCyclesDone(), SekPc);
    return d;
  }

  if (a==0x00) // data port
  {
    return VideoRead();
  }

  return 0;
}

unsigned char PicoVideoRead8DataH(void)
{
  return VideoRead() >> 8;
}

unsigned char PicoVideoRead8DataL(void)
{
  return VideoRead();
}

unsigned char PicoVideoRead8CtlH(void)
{
  u8 d = VideoSr(&Pico.video) >> 8;
  if (Pico.video.pending) {
    CommandChange();
    Pico.video.pending = 0;
  }
  elprintf(EL_SR, "SR read (h): %02x @ %06x", d, SekPc);
  return d;
}

unsigned char PicoVideoRead8CtlL(void)
{
  u8 d = VideoSr(&Pico.video);
  if (Pico.video.pending) {
    CommandChange();
    Pico.video.pending = 0;
  }
  elprintf(EL_SR, "SR read (l): %02x @ %06x", d, SekPc);
  return d;
}

unsigned char PicoVideoRead8HV_H(void)
{
  elprintf(EL_HVCNT, "vcounter: %02x [%u] @ %06x", Pico.video.v_counter, SekCyclesDone(), SekPc);
  return Pico.video.v_counter;
}

// FIXME: broken
unsigned char PicoVideoRead8HV_L(void)
{
  u32 d = (SekCyclesDone() - Pico.t.m68c_line_start) & 0x1ff; // FIXME
  if (Pico.video.reg[12]&1)
       d = hcounts_40[d];
  else d = hcounts_32[d];
  elprintf(EL_HVCNT, "hcounter: %02x [%u] @ %06x", d, SekCyclesDone(), SekPc);
  return d;
}

void PicoVideoSave(void)
{
  struct PicoVideo *pv = &Pico.video;
  int l, x;

  // account for all outstanding xfers XXX kludge, entry attr's not saved
  for (l = fifo_ql, x = fifo_qx + l-1; l > 1; l--, x--)
    pv->fifo_cnt += (fifo_queue[x&7] >> 2) << (fifo_queue[x&7] & 1);
}

void PicoVideoLoad(void)
{
  struct PicoVideo *pv = &Pico.video;
  int l;

  // convert former dma_xfers (why was this in PicoMisc anyway?)
  if (Pico.m.dma_xfers) {
    pv->fifo_cnt = Pico.m.dma_xfers * (pv->type == 1 ? 2 : 1);
    fifo_total = Pico.m.dma_xfers;
    Pico.m.dma_xfers = 0;
  }

  // rebuild SAT cache XXX wrong since cache and memory can differ
  for (l = 0; l < 80; l++) {
    *((u16 *)VdpSATCache + 2*l  ) = PicoMem.vram[(sat>>1) + l*4    ];
    *((u16 *)VdpSATCache + 2*l+1) = PicoMem.vram[(sat>>1) + l*4 + 1];
  }
}

// vim:shiftwidth=2:ts=2:expandtab
