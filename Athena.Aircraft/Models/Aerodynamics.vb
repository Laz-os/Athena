Imports Athena.Functions.Functions
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components.Basics
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components
Imports OpenVOGEL.DesignTools.VisualModel.Models
Imports OpenVOGEL.MathTools.Algebra.EuclideanSpace
Imports Athena.Console
Imports OpenVOGEL.AeroTools.CalculationModel.Solver
Imports Athena.Aircraft.Performance
Imports OpenVOGEL.AeroTools.CalculationModel
Imports OpenVOGEL.AeroTools.CalculationModel.Settings

Namespace Models
    Partial Public Class Design

        Public Shared Property MainWingID As Guid
        Public Shared Property ElevatorID As Guid
        Public Shared Property RudderID As Guid
        Public Shared Property FuselageID As Guid
        Public Shared Property WingRank As Integer
        Public Shared Property RudderRank As Integer
        Public Shared Property ElevatorRank As Integer
        Public Shared Property FuselageRank As Integer
        Public Shared Property Include_Fuselage As Boolean
        Public Shared Property Fuselage_length As Double
        Public Shared Property Lmac As Double
        Public Shared Property Loads As New List(Of AeroForces)

        Public Class Aerodynamics

            Public Sub TailDesign(ByRef score As Integer, Constrained As Boolean)
                Try
                    FinDesign(MyProjectRoot.Model)
                    Dim xx0 As Double = 0.3
                    Dim xx1 As Double = 0.6
                    Dim Re As Double = Model.PolarDataBase.Families(0).Polars(0).Reynolds
                    Dim ISA As New StandardAtmosphere(100)
                    MAC()
                    Dim Vel As Double = Re * ISA.KinematicVisc / Lmac

                    CalculationCommand.set_altitude(100)
                    CalculationCommand.set_velocity(Vel, 0, 0)
                    CalculationCommand.set_alfa(2)
                    SetTailIncidence(2, MyProjectRoot.Model)
                    Dim f0 As Double = TailSize(xx0, Constrained)
                    Dim f1 As Double = TailSize(xx1, Constrained)
                    Dim fr As Double
                    Dim root As Double = xx1 - f1 * (xx1 - xx0) / (f1 - f0)
                    If root < 0 Then
                        SetTailIncidence(-2, MyProjectRoot.Model)
                        f0 = TailSize(xx0, Constrained)
                        f1 = TailSize(xx1, Constrained)
                        root = xx1 - f1 * (xx1 - xx0) / (f1 - f0)
                        Dim length = CheckLength(root, f0, f1, Constrained)
                        fr = length.fr
                        If length.Cancel Then
                            score = 1
                            Exit Sub
                        End If
                    Else
                        Dim length = CheckLength(root, f0, f1, Constrained)
                        fr = length.fr
                    End If
                    While fr < 0
                        xx0 = xx1
                        xx1 = root
                        f0 = f1
                        f1 = fr
                        root = xx1 - f1 * (xx1 - xx0) / (f1 - f0)
                        Dim length = CheckLength(root, f0, f1, Constrained)
                        fr = length.fr
                        If length.Cancel Then
                            score = 1
                            Exit Sub
                        End If
                    End While
                Catch exce As Exception
                    score = 1
                    Exit Sub
                End Try

            End Sub

            Private Sub FinDesign(ByRef model As DesignModel)

                Dim length As Double
                For Each Surface As Surface In model.Objects
                    If Surface.Id = FuselageID Then
                        Dim fuselage As Fuselage = Surface
                        length = fuselage.CrossSections(fuselage.CrossSections.Count - 1).Z
                        Fuselage_length = length
                    End If
                Next

                For Each Surface As Surface In model.Objects
                    If Surface.Id = RudderID Then
                        Dim LiftingSurface As LiftingSurface = Surface
                        LiftingSurface.Position.X = length - LiftingSurface.RootChord - 0.05
                        LiftingSurface.GenerateMesh()
                    End If
                Next

            End Sub

            Public Sub SetTailIncidence(Inc As Double, ByRef model As DesignModel)
                Dim Elevator As LiftingSurface
                For Each Surface As Surface In model.Objects
                    If Surface.Id = ElevatorID Then
                        Elevator = Surface
                        Elevator.Orientation.Angle2 = Inc
                        Elevator.GenerateMesh()
                    End If
                Next
                LoadWingAnchorsToBody(model)
            End Sub

            Public Function TailSize(Length As Double, Constrained As Boolean)
                SetTailLength(Length, MyProjectRoot.Model, Constrained)
                Dim solver As New Solver
                'Steady(solver)
                MyProjectRoot.StartCalculation(Settings.CalculationType.SteadyState, solver)
                Dim aeroforces As New AeroForces
                aeroforces.Calculate(solver)
                Return StaticMargin(0.4, 0.2, aeroforces.R.X)
            End Function

            Private Sub SetTailLength(Lenght As Double, ByRef Model As DesignModel, Constrained As Boolean)
                Select Case Constrained
                    Case True

                        Dim L, PosY As Double
                        Dim Elevator As LiftingSurface

                        For Each Surface As Surface In Model.Objects
                            If Surface.Id = Models.Design.FuselageID Then
                                Dim fuselage As Fuselage = Surface
                                Dim NumSections As Integer
                                NumSections = fuselage.CrossSections.Count
                                L = fuselage.CrossSections(NumSections - 1).Z
                            End If
                        Next
                        Dim phi As Double = Math.Acos(L / 3)
                        For Each Surface As Surface In Model.Objects
                            If Surface.Id = ElevatorID Then
                                Elevator = Surface
                                PosY = Surface.Position.Y
                                For Each WingRegion As WingRegion In Elevator.WingRegions
                                    WingRegion.Length = Lenght
                                    WingRegion.Sweepback = 0
                                    WingRegion.TipChord = 0.02
                                    Elevator.RootChord = 0.02 + (Lenght / Math.Tan(phi))
                                    Elevator.Position.X = L - Elevator.RootChord - 0.05
                                Next
                                Elevator.GenerateMesh()
                            End If
                        Next

                    Case Else
                        Dim AR As Decimal = 6
                        Dim PosY, chord, Dihedral As Double
                        Dim Elevator As LiftingSurface
                        For Each Surface As Surface In Model.Objects
                            If Surface.Id = ElevatorID Then
                                Elevator = Surface
                                PosY = Surface.Position.Y
                                For Each WingRegion As WingRegion In Elevator.WingRegions
                                    WingRegion.Length = Lenght
                                    Dihedral = deg2rad(WingRegion.Dihedral)
                                    Dim span As Double = 2 * Math.Cos(Dihedral) * Lenght + 2 * PosY
                                    Dim area As Double = span ^ 2 / AR
                                    chord = (area / 2) / (Lenght + PosY)
                                    'If Constrained And Region.Sweepback <> 0 Then
                                    '    Elevator.RootChord = chord + (Region.Length / Math.Tan(deg2rad(Math.Abs(Region.Sweepback))))
                                    '    Region.Sweepback = 0
                                    '    Region.TipChord = chord
                                    '    Continue For
                                    'End If
                                    Elevator.RootChord = chord
                                    WingRegion.TipChord = chord
                                Next
                                Elevator.GenerateMesh()
                            End If
                        Next
                        ElevatorLongPosition(Model, chord)
                End Select

                LoadWingAnchorsToBody(Model)
            End Sub

            Private Sub ElevatorLongPosition(ByRef Model As DesignModel, chord As Double)
                'sets the position of the elevator at the max aft position in fuselage
                Dim length As Double
                For Each Surface As Surface In Model.Objects
                    If Surface.Id = FuselageID Then
                        Dim fuselage As Fuselage = Surface
                        length = fuselage.CrossSections(fuselage.CrossSections.Count - 1).Z
                    End If
                Next

                For Each surface As Surface In Model.Objects
                    If surface.Id = ElevatorID Then
                        Dim LiftingSurface As LiftingSurface = surface
                        LiftingSurface.Position.X = length - chord - 0.05
                        LiftingSurface.GenerateMesh()
                    End If
                Next

            End Sub

            Public Function CheckLength(root As Double, f0 As Double, f1 As Double, Constrained As Boolean) As (fr As Double, Cancel As Boolean)
                Dim fr As Double
                Dim Cancel As Boolean
                If root <= 0 Then
                    If f0 < 0 AndAlso f1 < 0 Then
                        Cancel = True
                        fr = 0
                    Else
                        If f1 > 0 Then
                            fr = f1
                            Cancel = False
                        End If
                        If f0 > 0 AndAlso f1 < 0 Then
                            fr = f0
                            Cancel = False
                        End If
                    End If
                    Return (fr, Cancel)
                Else
                    fr = TailSize(root, Constrained)
                    Cancel = False
                    Return (fr, Cancel)
                End If
            End Function

            Private Shared Function MAC() As (Xmac As Double, Ymac As Double, Lmac As Double)
                Dim Xmac As Double
                Dim Ymac As Double
                For Each Surface As Surface In MyProjectRoot.Model.Objects
                    If Surface.Id = MainWingID Then
                        Dim wing As LiftingSurface = Surface
                        Dim Cr As Double = wing.RootChord
                        Dim X As Double = wing.Position.X
                        Dim Y As Double = wing.Position.Y
                        For Each WingRegion As WingRegion In wing.WingRegions
                            Dim Sw As Double = WingRegion.Sweepback
                            Sw = deg2rad(Sw)
                            Dim L As Double = WingRegion.Length
                            Dim Ct As Double = WingRegion.TipChord
                            Dim t As Double = Ct / Cr
                            Lmac = Cr * (2 / 3) * ((1 + t + t ^ 2) / (1 + t))
                            Ymac = Y + L * ((1 + 2 * t) / (3 + 3 * t))
                            Xmac = X + Ymac * Math.Tan(Sw)
                        Next
                    End If
                Next
                Return (Xmac, Ymac, Lmac)
            End Function

            Public Shared Function StaticMargin(distance As Double, MACper As Double, RefPoint As Double) As Double
                Dim chord = MAC()
                Dim Xcg As Double = chord.Xmac + distance * chord.Lmac
                Dim margin As Double = RefPoint - Xcg
                Dim per As Double = MACper * chord.Lmac
                Dim SM As Double = margin - per
                Return SM
            End Function

            Public Function CheckDeflection(root As Double, g0 As Double, g1 As Double) As (defl As Double, Cancel As Boolean, Forces As AeroForces)
                Dim defl As Double
                Dim Cancel As Boolean

                If root < -60 Or root > 60 Then
                    If g0 > 0.01 AndAlso g1 > 0.01 Then
                        Cancel = True
                        defl = 0
                    Else
                        If g1 < 0.01 Then
                            defl = g1
                            Cancel = False
                        End If
                        If g0 < 0.01 AndAlso g1 > 0.01 Then
                            defl = g0
                            Cancel = False
                        End If
                    End If
                    Return (defl, Cancel, Nothing)
                ElseIf Double.IsNaN(root) Or Double.IsNaN(g0) Or Double.IsNaN(g1) Then
                    Cancel = True
                    Return (Nothing, Cancel, Nothing)
                Else
                    SetTailIncidence(root, MyProjectRoot.Model)
                    Dim solver As New Solver
                    Dim aeroforces As New AeroForces
                    Steady(solver)
                    aeroforces.Calculate(solver)
                    defl = StaticMargin(0.2, 0, aeroforces.R.X)
                    Cancel = False
                    Return (defl, Cancel, aeroforces)
                End If
            End Function

            Public Sub FuselageLength(ByRef model As DesignModel, MaxLength As Double)

                For Each Surface As Surface In model.Objects
                    If Surface.Id = Models.Design.FuselageID Then
                        Dim fuselage As Fuselage = Surface
                        Dim NumSections As Integer
                        NumSections = fuselage.CrossSections.Count
                        Dim differnce As Double = MaxLength - fuselage.CrossSections(NumSections - 1).Z

                        For i = Math.Floor(fuselage.CrossSections.Count / 2 - 1) To fuselage.CrossSections.Count - 1
                            fuselage.CrossSections(i).Z += differnce
                        Next
                        fuselage.GenerateMesh()
                    End If
                Next

            End Sub
#Region "Anchors"

            Public Sub LoadWingAnchorsToBody(ByRef Model As OpenVOGEL.DesignTools.VisualModel.Models.DesignModel)

                Dim Wings As New List(Of LiftingSurface)
                    Dim _Fuselage As Fuselage

                    For Each otherSurface In Model.Objects
                        If TypeOf otherSurface Is LiftingSurface Then
                            If otherSurface.Id = RudderID Then
                                Continue For
                            End If
                            Wings.Add(otherSurface)
                        ElseIf TypeOf otherSurface Is Fuselage Then
                            _Fuselage = otherSurface
                        End If
                    Next

                _Fuselage.AnchorLines.Clear()

                If Include_Fuselage Then

                    For i = 0 To Wings.Count - 1

                        Wings(i).GenerateMesh()

                        Dim AnchorLine As New AnchorLine

                        Dim n As Integer = Wings(i).NumberOfChordPanels

                        For j = 0 To n

                            Dim Line As New Line3()

                            Line.Point.Z = Wings(i).Mesh.Nodes(j).Position.X
                            Line.Point.Y = Wings(i).Mesh.Nodes(j).Position.Z
                            Line.Point.X = Wings(i).Mesh.Nodes(j).Position.Y

                            Dim pa As Vector3 = Wings(i).Mesh.Nodes(j).Position
                            Dim pb As Vector3 = Wings(i).Mesh.Nodes(j + n + 1).Position

                            Line.Direction.X = pa.Y - pb.Y
                            Line.Direction.Y = pa.Z - pb.Z
                            Line.Direction.Z = pa.X - pb.X
                            Line.Direction.Normalize()

                            AnchorLine.Lines.Add(Line)

                        Next

                        Dim Info As New WingAnchorInfo

                        Info.ParentID = Wings(i).Id

                        AnchorLine.WingAnchorInfo = Info

                        _Fuselage.AnchorLines.Add(AnchorLine)

                    Next

                End If

                _Fuselage.GenerateMesh()

            End Sub

#End Region

        End Class

        Public Class Engine

            Public Shared Function Thrust(V As Double)
                Dim a2 As Double = -1.5547 / 1000 * 9.81
                Dim a1 As Double = -12.423 / 1000 * 9.81
                Dim a0 As Double = 1186.9 / 1000 * 9.81
                Thrust = a2 * V ^ 2 + a1 * V + a0
                If Thrust < 0 Then
                    Thrust = 0
                End If
            End Function

        End Class

    End Class

End Namespace