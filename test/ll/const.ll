; ModuleID = 'calc'
source_filename = "<string>"

define i64 @c(i64 %x) {
entry:
  ret i64 1
}

define i64 @olifant() {
entry:
  %0 = call i64 @c(i64 42)
  ret i64 %0
}
