; ModuleID = 'calc'
source_filename = "<string>"

@x = global i64 42
@y = global i1 true

declare i64 @printi(i64)

define i64 @main(i64 %_) {
entry:
  %0 = load i64, i64* @x
  %1 = call i64 @printi(i64 %0)
  ret i64 %1
}
