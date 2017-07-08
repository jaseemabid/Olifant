; ModuleID = 'calc'
source_filename = "<string>"

declare i64 @printi(i64)

define i64 @id(i64 %x) {
entry:
  ret i64 %x
}

define i64 @main(i64 %_) {
entry:
  %0 = call i64 @id(i64 1)
  %1 = call i64 @printi(i64 %0)
  ret i64 %1
}
