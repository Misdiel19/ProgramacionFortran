subroutine Matriz(A, aw, h, g, l, M)
  implicit none

  real,intent(in):: A, aw, h, g, l
  real,dimension(2),intent(out):: M
  real:: ap, w, a2, w2
  real,dimension(2):: p1
  real,dimension(2):: p2

  ap = A
  W = aw
  a2 = h*w
  w2 = -h * g / l *a
  p1 = (/a, w/)
  p2 = (/a2, w2/)

  M = p1 + p2

end subroutine Matriz
