; ModuleID = 'calc'
source_filename = "<string>"

declare i64 @sum(i64, i64)

define i64 @olifant() {
entry:
  %0 = call i64 @sum(i64 4, i64 5)
  ret i64 %0
}
