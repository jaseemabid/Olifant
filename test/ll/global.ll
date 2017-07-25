; ModuleID = 'calc'
source_filename = "<string>"

@i = global i64 1
@j = global i1 true

define i64 @f(i64 %a, i1 %b) {
entry:
  ret i64 42
}

define i64 @olifant() {
entry:
  %0 = load i64, i64* @i
  %1 = load i1, i1* @j
  %2 = call i64 @f(i64 %0, i1 %1)
  ret i64 %2
}
