; ModuleID = 'calc'
source_filename = "<string>"

@a = global i64 1

define i64 @f(i64 %a) {
entry:
  ret i64 %a
}

define i64 @olifant() {
entry:
  %0 = load i64, i64* @a
  %1 = call i64 @f(i64 %0)
  ret i64 %1
}
