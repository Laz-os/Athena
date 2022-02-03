Namespace Models
    Partial Public Class Design
        Public Shared Property RudderArea As Double
        Public Shared Property ElevatorArea As Double
        Public Shared Property WingArea As Double
        Public Shared Property FuselageArea As Double
        Public Shared Property Payload As Double = 0

        Public Class Structural
            Public Shared Sub Weight(Sw As Double, Sr As Double, Se As Double, Sfus As Double, ByRef Properties As Performance.Mission.MissionProperties)
                Dim W_engine As Double = 0.955
                Dim W_servos As Double = 0.29
                Dim W_lg As Double = 0.135
                Dim W_rest As Double = 0.5
                Dim Wfus = 1.2 * Sfus
                Dim Wwing = 1.2 * Sw
                Dim We = 1.2 * Se
                Dim Wr = 1.2 * Sr
                'weight in N (=kg*10)
                Properties.TakeOffWeight = (W_engine + W_servos + W_lg + W_rest + Wfus + Wwing + We + Wr + Payload) * 10
            End Sub

            Public Shared Function SurfaceArea(Solver As OpenVOGEL.AeroTools.CalculationModel.Solver.Solver) As (Sw As Double, Sr As Double, Se As Double, Sfus As Double)
                Dim Sw, Sr, Se, Sfus As Double
                Sw = 4 * Solver.Lattices(WingRank).AirLoads.Area
                Sr = 2 * Solver.Lattices(RudderRank).AirLoads.Area
                Se = 4 * Solver.Lattices(ElevatorRank).AirLoads.Area
                If FuselageRank <= Solver.Lattices.Count - 1 Then
                    Sfus = Solver.Lattices(FuselageRank).AirLoads.Area
                Else
                    Sfus = 0
                End If

                Return (Sw, Sr, Se, Sfus)
            End Function

        End Class

        Public Class Inertia

        End Class

    End Class
End Namespace
