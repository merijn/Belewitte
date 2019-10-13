#!/usr/bin/env bash

exe=`basename "$0"`

usage () {
      printf "Usage:\n"
      printf "$exe file1 file2 [file3...]\n"
      printf "\nOptions:\n"
      printf "\t-h | --help\n"
      printf "\t\tThis help.\n"
      printf "\t-p PREC | --precision PREC\n"
      printf "\t\tSize of floating point error to ignore.\n"
      printf "\t-q | --quiet\n"
      printf "\t\tOnly print results in case of >0 errors.\n"
      printf "\t-v | --verbose\n"
      printf "\t\tPrint invidual errors exceeding threshold.\n"
      exit
}

if [ "$#" -lt 2 ]; then
    usage
fi

prec=0.0
args=()
file_args=""
n=0
v=1
while
  case "$1" in
    -h | --help)
      usage
      ;;
    -p | --precision)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      prec=$1
      ;;
    -v | --verbose)
      v=2
      ;;
    -q | --quiet)
      v=0
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
      args+=("$1")
      file_args+="\t$1"
      n=$(($n + 1))
      ;;
  esac
  shift 1
do
    :
done

paste $args | awk -v args="$file_args" -v num_args="$n" -v precision="$prec" -v verbose="$v" '
function abs(v) { return v < 0 ? -v : v }
BEGIN { count = 0; max_diff=0; split(args,files,"\t");}
{
    off=NF/num_args;
    for (i = 2; i <= off; i++) {
        for (j = 1; j < num_args; j++) {
            k = i + j*off
            diff = abs($i - $k);
            if (diff > precision) {
                if (diff > max_diff) {
                    max_diff = diff;
                }
                count++;
                if (verbose == 2) {
                    file_msg = ""
                    if (num_args > 2) {
                        file_msg = " (" files[1] " and " files[1+j] ")"
                    }
                    print "Line",NR,"field",i file_msg ":"
                    print $i,$k,"(difference:",diff ")"
                    print ""
                }
            }
        }
    }
}
END {
    if (verbose == 2) print""
    if (verbose || count > 0) print "Error count:",count"\tMax diff:",max_diff
}
'
