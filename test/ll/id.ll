; ModuleID = 'calc'
source_filename = "<string>"

define i64 @id(i64 %x) {
entry:
  ret i64 %x
}

define i64 @olifant() {
entry:
  %0 = call i64 @id(i64 42)
  ret i64 %0
}
