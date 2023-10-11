docker run --rm -ti --security-opt seccomp=unconfined -v $(pwd):/galamm \
 --platform linux/amd64 wch1/r-debug /bin/bash
