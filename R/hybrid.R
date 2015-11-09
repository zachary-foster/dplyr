hybrid <- function(call, data){
  global_subtitute( substitute(call), data, environment() )
}
