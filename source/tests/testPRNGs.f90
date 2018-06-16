Program testPRNGs

Use Kinds, Only: dp
Use PRNGs, Only: seed_rng_mt19937
Use PRNGs, Only: rng_mt19937

Implicit None

Real(dp) :: x
Integer :: i,unit

Open(NEWUNIT=unit,FILE='randoms.tst',ACTION='WRITE',STATUS='REPLACE')
Call seed_rng_mt19937(777)
Do i = 1,100000
    x = rng_mt19937()
    Write(unit,'(ES30.20)') x
End Do
Close(unit)

End Program testPRNGs