#! /bin/bash
EXT=native
corebuild -pkg core_bench -pkg ounit perf_test.$EXT list_world.$EXT list_world_test.$EXT
