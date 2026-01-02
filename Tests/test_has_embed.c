// Test __has_embed

// Existing file
#if __has_embed("test_embed.bin")
#define HAS_TEST 1
#else
#define HAS_TEST 0
#endif

// Non-existing file
#if __has_embed("nonexistent_file_12345.bin")
#define HAS_NOFILE 1
#else
#define HAS_NOFILE 0
#endif

// Empty file with limit(0)
#if __has_embed("test_embed.bin" limit(0)) == __STDC_EMBED_EMPTY__
#define LIMIT0_EMPTY 1
#else
#define LIMIT0_EMPTY 0
#endif

// Check specific values
#if __has_embed("test_embed.bin") == __STDC_EMBED_FOUND__
#define FOUND_EQ_1 1
#else
#define FOUND_EQ_1 0
#endif

#if __has_embed("nonexistent.bin") == __STDC_EMBED_NOT_FOUND__
#define NOTFOUND_EQ_0 1
#else
#define NOTFOUND_EQ_0 0
#endif

int test_has = HAS_TEST;
int test_nofile = HAS_NOFILE;
int test_limit0 = LIMIT0_EMPTY;
int test_found = FOUND_EQ_1;
int test_notfound = NOTFOUND_EQ_0;
