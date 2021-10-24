#!/usr/bin/env bash

exe=`basename "$0"`

usage () {
      printf "Usage:\n"
      printf "$exe file1 file2 [file3...]\n"
      printf "\nOptions:\n"
      printf "\t-h | --help\n"
      printf "\t\tThis help.\n"
      printf "\t-e | --exit-code\n"
      printf "\t\tReport number of errors as exit code.\n"
      printf "\t-a PREC | --abs-precision PREC\n"
      printf "\t\tSize of *absolute* floating point error to ignore.\n"
      printf "\t-r PREC | --rel-precision PREC\n"
      printf "\t\tSize of *relative* floating point error to ignore.\n"
      printf "\t-q | --quiet\n"
      printf "\t\tDon't print results.\n"
      printf "\t-v | --verbose\n"
      printf "\t\tPrint invidual errors exceeding threshold.\n"
      exit
}

if [ "$#" -lt 2 ]; then
    usage
fi

abs_prec=0.0
rel_prec=0.0
exit_code=0
args=()
file_args=""
n=0
v=1
while
  case "$1" in
    -h | --help)
      usage
      ;;
    -a | --abs-precision)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      abs_prec=$1
      ;;
    -r | --rel-precision)
      [ -z "$2" ] && printf "Missing argument for: $1\n\n" && usage || shift 1
      rel_prec=$1
      ;;
    -v | --verbose)
      v=2
      ;;
    -q | --quiet)
      v=0
      ;;
    -e | --exit-code)
      exit_code=1
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

paste "${args[@]}" | awk -v args="$file_args" -v num_args="$n" -v abs_precision="$abs_prec" -v rel_precision="$rel_prec" -v verbose="$v" -v exit_code="$exit_code" '
function abs(v) { return v < 0 ? -v : v }
BEGIN {
    count = 0;
    max_abs_diff=0;
    max_rel_diff=0;
    split(args,files,"\t");
}
{
    off=NF/num_args;
    for (i = 2; i <= off; i++) {
        for (j = 1; j < num_args; j++) {
            k = i + j*off
            abs_diff = abs($i - $k);
            rel_diff = abs_diff / $i;

            if (abs_diff > max_abs_diff) {
                max_abs_diff = abs_diff;
            }

            if (rel_diff > max_rel_diff) {
                max_rel_diff = rel_diff;
            }

            if (report(abs_diff, rel_diff)) {
                count++;
                if (verbose == 2) {
                    file_msg = ""
                    if (num_args > 2) {
                        file_msg = " (" files[1] " and " files[1+j] ")"
                    }
                    print "Line",NR,"field",i file_msg ":"
                    print $i,$k,"(abs diff:",abs_diff,"rel diff:",rel_diff ")"
                    print ""
                }
            }
        }
    }
}
END {
    if (verbose == 2) print ""
    if (verbose) {
        if (abs_precision != 0.0) {
            print "Max absolute error:",abs_precision
        }

        if (rel_precision != 0.0) {
            printf("Max relative error: %f (%.2f%%)\n", rel_precision, 100 * rel_precision);
        }

        print "Max abs diff:",max_abs_diff
        printf("Max rel diff: %f (%.2f%%)\n", max_rel_diff, 100 * max_rel_diff);
    }
    print "Error count:",count

    if (exit_code) {
        exit count
    }
}
function report (abs_diff_val, rel_diff_val) {
    if (abs_precision != 0.0 && rel_precision == 0.0) {
        return (abs_diff_val > abs_precision)
    } else if (abs_precision == 0.0 && rel_precision != 0.0) {
        return (rel_diff_val > rel_precision)
    } else {
        return (abs_diff_val > abs_precision || rel_diff_val > rel_precision)
    }
}
'
