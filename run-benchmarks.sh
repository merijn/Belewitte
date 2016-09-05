#!/usr/bin/env bash
exe=`basename "$0"`

usage () {
      printf "Options:\n"
      printf "\t-a | --algorithm\n"
      printf "\t\tAlgorithm(s) benchmark. (default: all)\n"
      printf "\t-h | --help\n"
      printf "\t\tThis help.\n"
      printf "\t-e EXT | --extension EXT\n"
      printf "\t\tExtension to use for algorithm output files.\n"
      printf "\t-l NAME | --label NAME\n"
      printf "\t\tName for result set\n"
      printf "\t-n NUM | --count NUM\n"
      printf "\t\tNumber of times to run algorithm.\n"
      printf "\t-r DIR | --result-dir DIR\n"
      printf "\t\tDirectory to write results too.\n"
      printf "\t-s | --save-output\n"
      printf "\t\tSave algorithm output.\n"
      printf "\t-t EXT | --timing-extension EXT\n"
      printf "\t\tExtension to use for algorithm timing files.\n"
      exit
}

algorithms=""
outputExtension="out"
n=30
resultdir="data"
label="results"
save=""
timingsExtension="timings"
while
  case $1 in
    -h | --help)
      usage
      ;;
    -a | --algorithm)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      if [ -n "$1" ]; then
          algorithms+="$1 "
      else
          printf "Zero length argument not allowed for $1!\n\n"
          usage
      fi
      ;;
    -e | --extension)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      if [ -n "$1" ]; then
          if [[ $1 == .* ]]; then
              outputExtension="${1#.}"
          else
              outputExtension="$1"
          fi
      else
          printf "Zero length argument not allowed for $1!\n\n"
          usage
      fi
      ;;
    -l | --label)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      label="$1"
      ;;
    -n | --count)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      n=$1
      ;;
    -r | --result-dir)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      resultdir="$1"
      ;;
    -s | --save-output)
      save="1"
      ;;
    -t | --timing-extension)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      if [ -n "$1" ]; then
          if [[ $1 == .* ]]; then
              timingsExtension="${1#.}"
          else
              timingsExtension="$1"
          fi
      else
          printf "Zero length argument not allowed for $1!\n\n"
          usage
      fi
      ;;
    --)
      shift 1
      args+=" $*"
      n=$(($n + $#))
      break
      ;;
    "") break
      ;;
    -*)
      printf "Unknown option: $1\n\n"
      usage
      ;;
    *)
      args+=" $1"
      n=$(($n + 1))
      ;;
  esac
  shift 1
do
    :
done

mkdir -p "$resultdir/$label"

for gpu in "TitanX"; do
    run () {
        prun -np 1 -native "-C $gpu" CUDA_VISIBLE_DEVICES="0,1,2,3,4,5,6,7,8,9,10" $*
    }

    if [ -z "$algorithms" ]; then
        algorithms=`./main list algorithms`
    fi

    for alg in $algorithms; do
        for impl in `./main list implementations -a $alg`; do
            extra_flags=""
            if [ -n "$save" ]; then
                extra_flags+="-s $alg.$impl.$gpu.$outputExtension "
                extra_falgs+="-o $resultdir/$label"
            fi
            run ./main -a $alg -k $impl -n $n -t $resultdir/$label/$alg.$impl.$gpu.$timingsExtension ../graphs/*.graph $extra_flags &
        done
    done
    wait
done

