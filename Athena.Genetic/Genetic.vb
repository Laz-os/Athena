Imports Athena.Functions.Functions
Imports Microsoft.Office.Interop

Public Class Genetic

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

    Delegate Function Fnc(X As Array, Y As Boolean) As (DistanceScore As Double, PayloadScore As Double, AltitudeScore As Double, TakeOffBonus As Double, Failed As Boolean)

    Public Function GenAl(ByRef Eval As Fnc, Constrained As Boolean)
        Randomize()
        Dim Xpop(Npop - 1, Nvar - 1) As Double
        Dim Fpop(Npop - 1) As Double
        Dim FpopTemp(Npop - 1, 5) As Double
        Dim Nelite2 As Integer = 0
        Dim XFpop(,) As Double = New Double(,) {}
        Dim Gbest(Ngen - 1, Nvar - 1) As Double
        Dim Fbest(Ngen - 1, 3)
        Dim DistanceBest, PayloadBest, AltitudeBest As Double
        DistanceBest = 0
        AltitudeBest = 0
        PayloadBest = 0


        Dim initialised As Boolean = True 'SET TRUE when an initialised population is to be inserted
        Select Case initialised
            Case True
                Dim xlApp As Excel.Application = New Excel.Application
                Dim xlWorkBook As Excel.Workbook = xlApp.Workbooks.Open("C:\Users\Laz-os\Desktop\New Data 1.xlsx")
                Dim xlWorkSheet As Excel.Worksheet = xlWorkBook.Worksheets("Sheet8")
                ReDim Xpop(Npop - 1, Nvar - 1)
                ReDim Fpop(Npop - 1)
                Dim count As Integer = 0
                For i = 1 To Npop

                    For j = 1 To Nvar
                        count += 1
                        Xpop(i - 1, j - 1) = xlWorkSheet.Range("C" & count).Value
                    Next
                    count += 1

                    FpopTemp(i - 1, 0) = xlWorkSheet.Range("E" & (i - 1) * 6 + 1).Value
                    If FpopTemp(i - 1, 0) > DistanceBest Then
                        DistanceBest = FpopTemp(i - 1, 0)
                    End If
                    FpopTemp(i - 1, 1) = xlWorkSheet.Range("E" & (i - 1) * 6 + 2).Value
                    If FpopTemp(i - 1, 1) > PayloadBest Then
                        PayloadBest = FpopTemp(i - 1, 1)
                    End If
                    FpopTemp(i - 1, 2) = xlWorkSheet.Range("E" & (i - 1) * 6 + 3).Value
                    If FpopTemp(i - 1, 2) > AltitudeBest Then
                        AltitudeBest = FpopTemp(i - 1, 2)
                    End If
                    FpopTemp(i - 1, 3) = xlWorkSheet.Range("E" & (i - 1) * 6 + 4).Value

                Next

                For k = 1 To Npop '- Nelite2
                    Fpop(k - 1) = (1000 * (FpopTemp(k - 1, 0) / DistanceBest + FpopTemp(k - 1, 1) / PayloadBest + FpopTemp(k - 1, 2) / AltitudeBest) / 3) * FpopTemp(k - 1, 3)
                Next

                xlApp.Workbooks.Close()
            Case False
                For i = 1 To Npop
                    For j = 1 To Nvar
                        Do
                            Xpop(i - 1, j - 1) = Rnd()
                        Loop Until Xpop(i - 1, j - 1) > 0
                    Next
                Next

        End Select

        For i = 1 To Ngen

            For k = 1 To Npop - Nelite2
                For j = 1 To Nvar
                    X2(j - 1) = Xpop(k - 1, j - 1) * (Xmax(j - 1) - Xmin(j - 1)) + Xmin(j - 1)
                    If j = (AirfoilIndex + 1) AndAlso AirfoilUse Then
                        X2(j - 1) = Math.Floor(X2(j - 1))
                    End If
                Next

                Dim Sround = Eval(X2, Constrained)

                If Sround.Failed = False Then
                    If Sround.DistanceScore > DistanceBest Then
                        DistanceBest = Sround.DistanceScore
                    End If
                    If Sround.PayloadScore > PayloadBest Then
                        PayloadBest = Sround.PayloadScore
                    End If
                    If Sround.AltitudeScore > AltitudeBest Then
                        AltitudeBest = Sround.AltitudeScore
                    End If
                End If
                FpopTemp(k - 1, 0) = Sround.DistanceScore
                FpopTemp(k - 1, 1) = Sround.PayloadScore
                FpopTemp(k - 1, 2) = Sround.AltitudeScore
                FpopTemp(k - 1, 3) = Sround.TakeOffBonus
                FpopTemp(k - 1, 4) = Sround.Failed

            Next

            For k = 1 To Npop '- Nelite2
                Select Case FpopTemp(k - 1, 4)
                    Case False
                        Fpop(k - 1) = (1000 * (FpopTemp(k - 1, 0) / DistanceBest + FpopTemp(k - 1, 1) / PayloadBest + FpopTemp(k - 1, 2) / AltitudeBest) / 3) * FpopTemp(k - 1, 3)
                        FpopTemp(k - 1, 5) = (1000 * (FpopTemp(k - 1, 0) / DistanceBest + FpopTemp(k - 1, 1) / PayloadBest + FpopTemp(k - 1, 2) / AltitudeBest) / 3) * FpopTemp(k - 1, 3)
                    Case True
                        Fpop(k - 1) = FpopTemp(k - 1, 3)
                        FpopTemp(k - 1, 5) = FpopTemp(k - 1, 3)
                End Select
            Next

            Nelite2 = Nelite
            ReDim XFpop(Xpop.GetUpperBound(0), Xpop.GetUpperBound(1) + 1)
            XFpop = ExpandArray(Xpop, 2, 1, 0)
            For m = 1 To Npop
                XFpop(m - 1, Nvar) = Fpop(m - 1)
            Next
            XFpop = SortThisArray(XFpop, Nvar, -1, False)
            FpopTemp = SortThisArray(FpopTemp, 5, -1, False)
            For m = 1 To Npop
                For j = 1 To Nvar + 1
                    If j = Nvar + 1 Then
                        Fpop(m - 1) = XFpop(m - 1, j - 1)
                    Else
                        Xpop(m - 1, j - 1) = XFpop(m - 1, j - 1)
                    End If
                Next
            Next
            Dim best = Fpop(Npop - 1)

            For j = 1 To Nvar
                Gbest(i - 1, j - 1) = Xpop(Npop - 1, j - 1)
            Next
            Fbest(i - 1, 0) = FpopTemp(Npop - 1, 0)
            Fbest(i - 1, 1) = FpopTemp(Npop - 1, 1)
            Fbest(i - 1, 2) = FpopTemp(Npop - 1, 2)
            Fbest(i - 1, 3) = FpopTemp(Npop - 1, 3)

            Dim Xpop2(,) As Double = Xpop.Clone()

            For j = 1 To Npop - Nelite
                Dim Fpop2() As Double = Fpop.Clone()
                Dim elem = RouletteWheelSelection(Fpop2)
                'If Fpop(elem.index1) > Fpop(elem.index2) Then
                '    Dim pp As Integer = elem.index1
                '    elem.index1 = elem.index2
                '    elem.index2 = pp
                'End If

                For k = 1 To Nvar
                    'Dim a1 As Double = Rnd()
                    'Xpop2(j - 1, k - 1) = a1 * (Xpop(elem.index2, k - 1) - Xpop(elem.index1, k - 1)) + Xpop(elem.index2, k - 1)
                    'If Xpop2(j - 1, k - 1) >= 1 OrElse Xpop2(j - 1, k - 1) <= 0 Then
                    '    Dim b As Double = Rnd()
                    '    Xpop2(j - 1, k - 1) = b * (Xpop(elem.index2, k - 1) - Xpop(elem.index1, k - 1)) + Xpop(elem.index2, k - 1)
                    'End If
                    'Xpop2(j - 1, k - 1) = Math.Max(Xpop2(j - 1, k - 1), 0)
                    'Xpop2(j - 1, k - 1) = Math.Min(Xpop2(j - 1, k - 1), 1)

                    Dim c1 As Double = Math.Min(Xpop(elem.index2, k - 1), Xpop(elem.index1, k - 1))
                    Dim c2 As Double = Math.Max(Xpop(elem.index2, k - 1), Xpop(elem.index1, k - 1))

                    Dim Lbound As Double = c1 - 0.5 * (c2 - c1)
                    Dim Ubound As Double = c2 + 0.5 * (c2 - c1)
                    Xpop2(j - 1, k - 1) = Unifrnd(Lbound, Ubound)
                    If Xpop2(j - 1, k - 1) >= 1 OrElse Xpop2(j - 1, k - 1) <= 0 Then
                        Xpop2(j - 1, k - 1) = Unifrnd(c1, c2)
                    End If
                Next
            Next
            Xpop = Xpop2

            For j = 1 To Npop - Nelite
                Dim a2 As Double = Rnd()
                If a2 <= Pmut Then
                    Dim a1 As Integer = Math.Floor(Rnd() * Nvar) + 1
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
        For j = 1 To Nvar
            X2(j - 1) = Xpop(Npop - 1, j - 1) * (Xmax(j - 1) - Xmin(j - 1)) + Xmin(j - 1)
            If j = (AirfoilIndex + 1) AndAlso AirfoilUse Then
                X2(j - 1) = Math.Floor(X2(j - 1))
            End If
        Next
        Return X2
    End Function

    Private Function RouletteWheelSelection(ar() As Double) As (index1 As Integer, index2 As Integer)
        Dim n As Double = 0
        Dim minimun As Double = Math.Abs(Min(ar))
        ar(0) += (minimun + 5)
        For i = 1 To ar.GetUpperBound(0)
            ar(i) += ar(i - 1) + (minimun + 5)
        Next
        Dim r As Double = Unifrnd(Min(ar), Max(ar))
        Dim index1 As Integer = Array.FindIndex(ar, Function(value) value >= r)
        r = Unifrnd(Min(ar), Max(ar))
        Dim index2 As Integer = Array.FindIndex(ar, Function(value) value >= r)
        Return (index1, index2)
    End Function


End Class
