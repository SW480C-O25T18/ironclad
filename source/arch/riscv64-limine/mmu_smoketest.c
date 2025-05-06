/* mmu_smoketest.c – build a one-page Sv39 table and print ‘!’ */
#include <stdint.h>

#define UART_PHYS   0x10000000ULL        // QEMU virt UART
#define UART_VIRT   0xFFFFFFFF00000000ULL// pick any high address

/* Each RISC-V PTE bit */
#define PTE_V  (1ULL<<0)
#define PTE_R  (1ULL<<1)
#define PTE_W  (1ULL<<2)
#define PTE_X  (1ULL<<3)
#define PTE_A  (1ULL<<6)
#define PTE_D  (1ULL<<7)

/* 4096-byte page for the three levels */
static uint64_t l2[512] __attribute__((aligned(4096)));
static uint64_t l1[512] __attribute__((aligned(4096)));
static uint64_t l0[512] __attribute__((aligned(4096)));

static inline void write_satp(uint64_t v)
{
    asm volatile ("csrw satp, %0   \n"
                  "sfence.vma      \n" :: "r"(v));
}

void _start(void)
{
    /* ---------- build the tiny page tree ---------- */
    uint64_t ppn_uart = UART_PHYS >> 12;

    /* leaf PTE for UART page: R/W */
    l0[(UART_VIRT >> 12) & 0x1ff] =
        (ppn_uart << 10) | PTE_V | PTE_R | PTE_W | PTE_A | PTE_D;

    /* point L1 entry at L0 */
    l1[(UART_VIRT >> 21) & 0x1ff] =
        ((uint64_t)l0 >> 2) | PTE_V;

    /* point L2 entry at L1 */
    l2[(UART_VIRT >> 30) & 0x1ff] =
        ((uint64_t)l1 >> 2) | PTE_V;

    /* ---------- turn the MMU on ---------- */
    uint64_t satp = 0;
    uint64_t root_ppn = ((uint64_t)l2) >> 12;   // physical page number
    satp |= (8ULL   << 60);                     // MODE = 8 (Sv39)
    satp |= (0ULL   << 44);                     // ASID = 0
    satp |=  root_ppn;                          // PPN  = root table
    write_satp(satp);

    /* ---------- write to the VIRTUAL UART ---------- */
    volatile uint8_t *vuart = (uint8_t *)UART_VIRT;
    *vuart = '!';               // prints ‘!’ if mapping works

    /* power-off via SBI so qemu exits */
    register long a7 asm("a7") = 0x53525354; // SBI_EXT_TEST (any invalid)
    asm volatile ("ecall");
    while (1) ;
}
