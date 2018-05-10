/**
 * csim - cache simulator
 * 
 * Taylor Rainwater - rainwatertr
 */

#include "cachelab.h"

/**
 * _help - prints help message
 */
void _help()
{   char help_text[] = "\
      -h: Optional help flag that prints usage info\n\
      -v: Optional verbose flag that displays trace info\n\
      -s <s>: Number of set index bits (S = 2^s is the number of sets)\n\
      -E <E>: Associativity (number of lines per set)\n\
      -b <b>: Number of block bits (B = 2^b is the block size)\n\
      -t <tracefile>: Name of the valgrind trace to replay";
    printf("%s\n", help_text);
}

/**
 * _error - prints error message
 */
void _error()
{
    printf("!--INCORRECT FLAG ENCOUNTERED--!\n");
    _help();
}

/** 
 * _init_cache - initialize the cache object
 * 
 *   takes: Cache, ul, ul, ul, char*, bool, bool
 * returns: nothing
 */ 
void _init_cache(Cache *mycache, unsigned long sets,
		 unsigned long associativity, unsigned long block_bits,
		 char *file, bool verbose, bool debug)
{   int i;
    int j;
    // Init all values in cache
    mycache->tag_bits = (64 - (sets + block_bits));
    mycache->set_bits = sets;
    mycache->block_bits = block_bits;
    mycache->associativity = associativity;
    mycache->file = file;
    mycache->verbose = verbose;
    mycache->debug = debug;
    // Init hits, misses, and evictions
    mycache->hits = 0;
    mycache->misses = 0;
    mycache->evictions = 0;
    // Init tag array with malloc
    mycache->tags = (unsigned long **) malloc(pow(2, sets));
    for (i = 0; i < pow(2, sets); i++){
        mycache->tags[i] = (unsigned long *) malloc(associativity);
	for (j = 0; j < associativity; j++){
	    mycache->tags[i][j] = -1;
	}
    }
}

/**
 * _read_line - reads a line from trace file
 * 
 *   takes: FILE, char*, *ul, *ul
 * returns: int
 */
int _read_line(FILE *trace_file, char *opt, unsigned long *addr,
	      unsigned long *size)
{   char line[BUFLEN];
    if (trace_file == NULL){
	perror("!--ERROR OPENING FILE--!");
	return(-1);
    }

    if (fgets(line, 80, trace_file) != NULL){
	sscanf(line, "%s %lu,%lu", opt, addr, size);	
    }
    return 0;
}

/** 
 * _get_between - gets bits from address
 * 
 *   takes: ul, ul, ul
 * returns: ul
 */
unsigned long _get_between(unsigned long addr, unsigned long begin,
			   unsigned long end)
{
    return (unsigned long) (addr >> begin) & ((1 << (end - begin)) - 1);
}

/**
 * build_struct - build a new cache object and return
 *
 *   takes: int, char *
 * returns: new Cache object
 */
Cache build_struct(int argc, char * argv[])
{   int i;
    bool debug = false; 
    bool verbose = false; 
    unsigned long sets = 0;
    unsigned long associativity = 0;
    unsigned long block_bits = 0;
    char *file;
    Cache mycache;
    // check for flags 
    for(i = 1; i < argc; i++){
        char curr = argv[i][1];
	switch(curr){
	case 'h':
	    _help();
	    break;
	case 'v':
	    verbose = true;
	    break;
	case 's':
	    sets = strtoul(argv[++i], NULL, 0);
	    break;
	case 'E':
	    associativity = strtoul(argv[++i], NULL, 0);
	    break;
	case 'b':
	    block_bits = strtoul(argv[++i], NULL, 0);
	    break;
	case 't':
	    file = malloc(sizeof(*argv[++i]));
	    file = argv[++i];
	    break;
	case 'd':
	    debug = true;
	    break;
	default:
	    if (isdigit(*argv[i])) { break; }
	    _error();
	    exit(1);
	}
    }
    _init_cache(&mycache, sets, associativity, block_bits, file,
		verbose, debug);
    // debugging
    if (debug){
	printf("SETS: %lu, ASSCO: %lu, BLOCK BITS: %lu, FILE NAME: %s\n",
	       sets, associativity, block_bits, file);
    }
    // return the baby cache, fresh and new
    return mycache;
}

void insert_cache(Cache *mycache, char *opt,
		  unsigned long set, unsigned long tag,
		  unsigned long addr, unsigned long size)
{   int i;
    for (i = 0; i < mycache->associativity; i++){
	if (mycache->tags[set][i] == -1){
	    mycache->tags[set][i] = tag;
	    mycache->misses += 1;
	    if (mycache->verbose) { printf("%s %lu,%lu miss\n",
					   opt, addr, size); }
	    break;
	} else if (mycache->tags[set][i] == tag) {
	    mycache->hits += 1;
	    if (mycache->verbose) { printf("%s %lu,%lu hit\n",
					   opt, addr, size); }
	    break;
	} else if (mycache->tags[set][i] != tag){
	    mycache->misses += 1;
	    mycache->evictions += 1;
	    if (mycache->verbose) { printf("%s %lu,%lu miss eviction\n",
					   opt, addr, size); }
	    mycache->tags[set][i] = tag;
	    break;
	}
    }
}

int run(Cache *mycache, FILE *trace_file)
{   int error = 0;
    char *opt = (char *) malloc(BUFLEN);
    unsigned long addr = 0, size = 0, tag = 0, set = 0;

    while(feof(trace_file) == 0){
	error = _read_line(trace_file, opt, &addr, &size);
	if (mycache->debug) {
	    printf("OPT: %s %lu,%lu\n", opt, addr, size);
	}
	// get set and tag bits
	set = (addr << mycache->tag_bits) >> (mycache->tag_bits + mycache->block_bits);
	tag = (addr >> (mycache->set_bits + mycache->block_bits));
	insert_cache(mycache, opt, set, tag, addr, size);
	if (mycache->debug) {
	    printf("SET: %lu, TAG: %lu\n", set, tag);
	}
    }
    return error;
}

int main(int argc, char * argv[])
{   int error = 0;
    Cache mycache;
    // Time to do things
    mycache = build_struct(argc, argv);
    // Open trace file
    FILE *trace_file = fopen(mycache.file, "r");    
    // Run the simulator
    error = run(&mycache, trace_file);
    // pass to printSummary the number of hits, misses and evictions
    printSummary(mycache.hits, mycache.misses, mycache.evictions);
    fclose(trace_file);
    return error;
}
