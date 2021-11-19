Imports Athena.Functions.Functions

Public Class Genetic

    Public Property XGbest As Double() = New Double() {}
    Public Property Ngen As Integer
    Public Property Npop As Integer
    Public Property Nelite As Integer
    Public Property Pmut As Double
    Public Property Nvar As Integer
    Public Property Xmin As Double() = New Double() {}
    Public Property Xmax As Double() = New Double() {}
    Public Property X2 As Double() = New Double() {}
    Public Property AirfoilIndex As Double
    Public Property AirfoilUse As Boolean = False

    Delegate Function Fnc(X As Array, Y As Boolean) As (DistanceScore As Double, PayloadScore As Double, AltitudeScore As Double, TakeOffBonus As Double)

    Public Sub GenAl(ByRef Eval As Fnc, Constrained As Boolean)
        Randomize()
        Dim Xpop(Npop - 1, Nvar - 1) As Double
        For i = 1 To Npop
            For j = 1 To Nvar
                Do
                    Xpop(i - 1, j - 1) = Rnd()
                Loop Until Xpop(i - 1, j - 1) > 0
            Next
        Next
        Dim Fpop(Npop - 1) As Double
        Dim FpopTemp(Npop - 1, 3) As Double '= New Double(,) {}
        Dim Nelite2 As Integer = 0
        Dim XFpop(,) As Double = New Double(,) {}
        Dim Gbest As Double

        For i = 1 To Ngen
            Dim DistanceBest, PayloadBest, AltitudeBest As Double
            DistanceBest = 0.0123
            AltitudeBest = 0.0123
            PayloadBest = 0.0123 '0.0123 to avoid division by 0 at line 70 

            For k = 1 To Npop - Nelite2
                For j = 1 To Nvar
                    X2(j - 1) = Xpop(k - 1, j - 1) * (Xmax(j - 1) - Xmin(j - 1)) + Xmin(j - 1)
                    If j = (AirfoilIndex + 1) AndAlso AirfoilUse Then
                        X2(j - 1) = Math.Floor(X2(j - 1))
                    End If
                Next
                Try
                    Dim Sround = Eval(X2, Constrained)

                    If Sround.DistanceScore > DistanceBest Then
                        DistanceBest = Sround.DistanceScore
                    End If
                    If Sround.PayloadScore > PayloadBest Then
                        PayloadBest = Sround.PayloadScore
                    End If
                    If Sround.AltitudeScore > AltitudeBest Then
                        AltitudeBest = Sround.AltitudeScore
                    End If
                    FpopTemp(k - 1, 0) = Sround.DistanceScore
                    FpopTemp(k - 1, 1) = Sround.PayloadScore
                    FpopTemp(k - 1, 2) = Sround.AltitudeScore
                    FpopTemp(k - 1, 3) = Sround.TakeOffBonus
                Catch ex As ArgumentOutOfRangeException
                    FpopTemp(k - 1, 0) = 0
                    FpopTemp(k - 1, 1) = 0
                    FpopTemp(k - 1, 2) = 0
                    FpopTemp(k - 1, 3) = 0
                End Try
            Next
            For k = 1 To Npop - Nelite2
                Fpop(k - 1) = (1000 * (FpopTemp(k - 1, 0) / DistanceBest + FpopTemp(k - 1, 1) / PayloadBest + FpopTemp(k - 1, 2) / AltitudeBest) / 3) * FpopTemp(k - 1, 3)
            Next

            Nelite2 = Nelite
            ReDim XFpop(Xpop.GetUpperBound(0), Xpop.GetUpperBound(1) + 1)
            XFpop = ExpandArray(Xpop, 2, 1, 0)
            For m = 1 To Npop
                XFpop(m - 1, Nvar) = Fpop(m - 1)
            Next
            XFpop = SortThisArray(XFpop, Nvar, -1, False) '
            For m = 1 To Npop
                For j = 1 To Nvar + 1
                    If j = Nvar + 1 Then
                        Fpop(m - 1) = XFpop(m - 1, j - 1)
                    Else
                        Xpop(m - 1, j - 1) = XFpop(m - 1, j - 1)
                    End If
                Next
            Next
            Gbest = Fpop(Npop - 1)

            Dim WorstF As Double = 1
            ReDim XGbest(Nvar - 1)
            For j = 1 To Nvar
                XGbest(j - 1) = Xpop(Npop - 1, j - 1) * (Xmax(j - 1) - Xmin(j - 1)) + Xmin(j - 1)
                If j = (AirfoilIndex + 1) AndAlso AirfoilUse Then
                    XGbest(j - 1) = Math.Round(XGbest(j - 1))
                End If
            Next
            Dim Xpop2(,) As Double = Xpop

            For j = 1 To Npop - Nelite
                Dim elem = RouletteWheelSelection(Fpop)
                If Fpop(elem.index1) < Fpop(elem.index2) Then
                    Dim pp As Integer = elem.index1
                    elem.index1 = elem.index2
                    elem.index2 = pp
                End If
                Randomize()
                For k = 1 To Nvar
                    Dim a1 As Double = Unifrnd(-0.4, 1.4)
                    Xpop2(j - 1, k - 1) = a1 * (Xpop(elem.index2, k - 1) - Xpop(elem.index1, k - 1)) + Xpop(elem.index2, k - 1)
                    If Xpop2(j - 1, k - 1) > 1 OrElse Xpop2(j - 1, k - 1) < 0 Then
                        Dim b As Double = Rnd()
                        Xpop2(j - 1, k - 1) = b * Math.Abs(Xpop(elem.index1, k - 1) - Xpop(elem.index2, k - 1)) + Xpop(elem.index2, k - 1)
                    End If
                    Xpop2(j - 1, k - 1) = Math.Max(Xpop2(j - 1, k - 1), 0)
                    Xpop2(j - 1, k - 1) = Math.Min(Xpop2(j - 1, k - 1), 1)
                Next
            Next
            Xpop = Xpop2
            Randomize()
            For j = 1 To Npop - Nelite
                Dim a2 As Double = Rnd()
                If a2 <= Pmut Then
                    Dim a1 As Integer = Math.Floor(Rnd() * (Nvar - 1)) + 1
                    Xpop(j - 1, a1 - 1) = Rnd()
                End If
            Next
        Next
        XFpop = ExpandArray(Xpop, 2, 1, 0)
        For m = 1 To Npop
            XFpop(m - 1, Nvar) = Fpop(m - 1)
        Next
        XFpop = SortThisArray(XFpop, Nvar, -1, False)
        For m = 1 To Npop
            For j = 1 To Nvar + 1
                If j = Nvar + 1 Then
                    Fpop(m - 1) = XFpop(m - 1, j - 1)
                Else
                    Xpop(m - 1, j - 1) = XFpop(m - 1, j - 1)
                End If
            Next
        Next
        Gbest = Fpop(Npop - 1)
    End Sub

    Private Function RouletteWheelSelection(ar() As Double) As (index1 As Integer, index2 As Integer)
        Dim n As Double = 0
        Dim minimun As Double = Math.Abs(Min(ar))
        For i = 0 To ar.GetUpperBound(0)
            ar(i) += n + (minimun + 5)
            n = ar(i)
        Next
        Dim r As Double = Unifrnd(Min(ar), Max(ar))
        Dim index1 As Integer = Array.FindIndex(ar, Function(value) value >= r)
        r = Unifrnd(Min(ar), Max(ar))
        Dim index2 As Integer = Array.FindIndex(ar, Function(value) value >= r)
        Return (index1, index2)
    End Function


End Class
