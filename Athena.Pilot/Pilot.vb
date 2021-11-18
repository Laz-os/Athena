Imports OpenVOGEL.DesignTools.VisualModel.Models.Components.Basics
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components
Imports Athena.Aircraft.Models.Design
Imports Athena.Functions.Functions
Imports Athena.Aircraft
Imports System.Net
Imports System.Net.Sockets
Imports System.Text
Imports Athena.Console
Imports OpenVOGEL.AeroTools.CalculationModel.Settings


Public Module Pilot

    Public Property DesignParameters As New List(Of String)

    Public Function StartCalculation(X2 As Array, Constrained As Boolean) As (DistanceScore As Double, PayloadScore As Double, AltitudeScore As Double, BonusTakeOff As Double)
        Dim DistanceScore As Double = 0
        Dim PayloadScore As Double = 0
        Dim AltitudeScore As Double = 0
        Dim score As Integer = 0
#Region "tail sizing"
        Dim Aero As New Aerodynamics

        ''' Defines whether the FUSELAGE will be taken into account in the calculation
        Models.Design.Include_Fuselage = False
        For Each Surface As Surface In Model.Objects
            If Surface.Id = Models.Design.FuselageID Then
                Surface.IncludeInCalculation = False
            End If
        Next
        '''
        '!!!! Constrained should be set to TRUE only if we have a PARAMETRIC definition of input parameters ( spanf=f(fus_lenth) )!!!!
        SetDesignVariables(X2, MyProjectRoot.Model, Aero, False)
        '''
        If Constrained Then
            Check_ACC_Constrains(score, MyProjectRoot.Model)
            If score < 0 Then
                Return (0, 0, 0, 0)
            End If
        End If
        Aero.TailDesign(score, Constrained)
        If score = 1 Then
            Return (1, 1, 1, 1)
        End If
        Loads.Clear()
#End Region
#Region "polar curves "
        Dim Re As Double = Model.PolarDataBase.Families(0).Polars(0).Reynolds
        Dim ISA As New StandardAtmosphere(100)
        Dim Vel As Double = Re * ISA.KinematicVisc / Lmac

        Dim Alt = New Double() {100} '{130, 0, 65, 0}
        Dim mach = New Double() {Vel / ISA.SoundSpeed} '{0.005, 0.005, 0.0275, 0.05}


        For i = 0 To UBound(Alt, 1) 'Each altitude In Alt
            Performance.DragPolar(Alt(i), mach(i))
        Next

#End Region
        Dim Mission As New Performance.Mission
        Dim Properties As New Performance.Mission.MissionProperties
        Try
            Mission.Initialize(Properties, Alt.Length, Vel, ISA.Density)
            Mission.TakeOff(Properties)
            Mission.Climb(Properties)
            Mission.ConstantEnergyCruise(Properties)
            Mission.DescentCruise(Properties)
        Catch ex As ArgumentOutOfRangeException
            score = 0
            Return (0, 0, 0, 0)
        End Try
        'Dim Sround As Double = ((Payload + Properties.DistanceScore + Properties.AltitudeScore) / 3) * Properties.TakeOffBonus
        Return (Properties.DistanceScore, Payload, Properties.AltitudeScore, Properties.TakeOffBonus)
    End Function


    Public Sub SetDesignVariables(X2 As Array, ByRef Model As OpenVOGEL.DesignTools.VisualModel.Models.DesignModel, ByRef Aero As Aerodynamics, Constrained As Boolean)
        If Constrained Then
            Randomize()
            Dim i, L, WingPosX, Ct, Cr As Double
            For Each Surface As Surface In Model.Objects
                If Surface.Id = MainWingID Then
                    Dim LiftingSurface As LiftingSurface = Surface
                    If DesignParameters.Contains(FormPilot.CheckBox2.Text) Then
                        i = DesignParameters.IndexOf(FormPilot.CheckBox2.Text)
                        LiftingSurface.WingRegions(0).TipChord = X2(i)
                        Ct = X2(i)
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox5.Text) Then
                        i = DesignParameters.IndexOf(FormPilot.CheckBox5.Text)
                        LiftingSurface.Orientation.Angle2 = X2(i)
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox6.Text) Then
                        i = DesignParameters.IndexOf(FormPilot.CheckBox6.Text)
                        LiftingSurface.WingRegions(0).CamberLineId = OpenVOGEL.DesignTools.DataStore.CamberLinesDatabase.CamberLines(X2(i)).ID
                        LiftingSurface.WingRegions(0).PolarId = Model.PolarDataBase.Families(X2(i)).Id
                        LiftingSurface.WingRegions(0).PolarFamiliy = Model.PolarDataBase.Families(X2(i))
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox8.Text) Then
                        i = DesignParameters.IndexOf(FormPilot.CheckBox8.Text)
                        Athena.Aircraft.Models.Design.Payload = X2(i)
                    End If
                    LiftingSurface.GenerateMesh()
                ElseIf Surface.Id = FuselageID Then
                    If DesignParameters.Contains(FormPilot.CheckBox9.Text) Then
                        Aero.FuselageLength(Model, X2(X2.GetUpperBound(0)))
                        L = X2(X2.GetUpperBound(0))
                    End If
                End If
            Next

            For Each Surface As Surface In Model.Objects
                If Surface.Id = MainWingID Then
                    Dim LiftingSurface As LiftingSurface = Surface
                    If DesignParameters.Contains(FormPilot.CheckBox1.Text) Then
                        LiftingSurface.RootChord = Rnd() * (L / 2 - Ct) + Ct
                        Cr = LiftingSurface.RootChord
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox7.Text) Then
                        LiftingSurface.Position.X = Rnd() * ((L / 2 - 0.05) - Cr / 2) + (Cr / 2)
                        WingPosX = LiftingSurface.Position.X
                    End If

                    Dim phi As Double = Math.Asin((L + 0.1) / 3)
                    Dim S As Double = 0.5 * Ct / Math.Sin(phi)
                    'If DesignParameters.Contains(FormPilot.CheckBox3.Text) Then
                    LiftingSurface.WingRegions(0).Length = 1.5 * Math.Cos(phi) - S - 0.053 '0.053 IS THE RADIOUS OF THE FUSELAGE
                    'End If
                    'If DesignParameters.Contains(FormPilot.CheckBox4.Text) Then
                    LiftingSurface.WingRegions(0).Sweepback = rad2deg(Math.Atan((WingPosX - S * Math.Tan(phi)) / (-0.053 - (-LiftingSurface.WingRegions(0).Length))))
                    'End If
                    LiftingSurface.GenerateMesh()
                End If
            Next

            Aero.LoadWingAnchorsToBody(Model)
        Else
            Dim i As Integer = 0
            For Each Surface As Surface In Model.Objects

                If Surface.Id = MainWingID Then
                    Dim LiftingSurface As LiftingSurface = Surface

                    If DesignParameters.Contains(FormPilot.CheckBox1.Text) Then
                        LiftingSurface.RootChord = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox2.Text) Then
                        LiftingSurface.WingRegions(0).TipChord = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox3.Text) Then
                        LiftingSurface.WingRegions(0).Length = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox4.Text) Then
                        LiftingSurface.WingRegions(0).Sweepback = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox5.Text) Then
                        LiftingSurface.Orientation.Angle2 = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox6.Text) Then
                        LiftingSurface.WingRegions(0).CamberLineId = OpenVOGEL.DesignTools.DataStore.CamberLinesDatabase.CamberLines(X2(i)).ID
                        LiftingSurface.WingRegions(0).PolarId = Model.PolarDataBase.Families(X2(i)).Id
                        LiftingSurface.WingRegions(0).PolarFamiliy = Model.PolarDataBase.Families(X2(i))
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox7.Text) Then
                        LiftingSurface.Position.X = X2(i)
                        i += 1
                    End If
                    If DesignParameters.Contains(FormPilot.CheckBox8.Text) Then
                        Athena.Aircraft.Models.Design.Payload = X2(i)
                    End If
                    LiftingSurface.GenerateMesh()

                ElseIf Surface.Id = FuselageID Then

                    If DesignParameters.Contains(FormPilot.CheckBox9.Text) Then
                        Aero.FuselageLength(Model, X2(X2.GetUpperBound(0)))
                    End If
                End If

            Next
            Aero.LoadWingAnchorsToBody(Model)
        End If

    End Sub

    Private Sub Check_ACC_Constrains(ByRef score As Integer, ByVal model As OpenVOGEL.DesignTools.VisualModel.Models.DesignModel)

        Dim L, WingPosX, Ct, Cr As Double
        For Each Surface As Surface In model.Objects
            If Surface.ID = MainWingID Then
                Dim LiftingSurface As LiftingSurface = Surface
                Ct = LiftingSurface.WingRegions(0).TipChord
            ElseIf Surface.ID = FuselageID Then
                Dim fuselage As Fuselage = Surface
                L = fuselage.CrossSections(fuselage.CrossSections.Count - 1).Z
            End If
        Next

        For Each Surface As Surface In model.Objects
            If Surface.ID = MainWingID Then
                Dim LiftingSurface As LiftingSurface = Surface

                Cr = LiftingSurface.RootChord
                If Cr < Ct Then
                    score -= ((Ct - Cr) / (L / 3 - Ct)) * 100
                ElseIf Cr > (L / 3) Then
                    score -= ((Cr - L / 3) / (L / 3 - Ct)) * 100
                End If

                WingPosX = LiftingSurface.Position.X
                If WingPosX < (0.25 * L) Then
                    score -= (0.25 * L - WingPosX) / (((0.75 * L - 0.15) - (0.25 * L)) * 100)
                ElseIf WingPosX > (0.75 * L - 0.15) Then
                    score -= (WingPosX - (0.75 * L - 0.15)) / (((0.75 * L - 0.15) - (0.25 * L)) * 100)
                End If

                'Dim phi As Double = Math.Asin((L + 0.1) / 3)
                'Dim S As Double = 0.5 * Ct / Math.Sin(phi)
                'If LiftingSurface.WingRegions(0).Length < 0.1 Then '0.053 IS THE RADIOUS OF THE FUSELAGE
                '    score -= (0.1 - LiftingSurface.WingRegions(0).Length) / ((1.5 * Math.Cos(phi) - S - 0.053) - 0.1) * 100
                'ElseIf LiftingSurface.WingRegions(0).Length > (1.5 * Math.Cos(phi) - S - 0.053) Then
                '    score -= (LiftingSurface.WingRegions(0).Length - (1.5 * Math.Cos(phi) - S - 0.053)) / ((1.5 * Math.Cos(phi) - S - 0.053) - 0.1) * 100
                'End If

                'If LiftingSurface.WingRegions(0).Sweepback > rad2deg(Math.Atan((WingPosX - S * Math.Tan(phi)) / (-0.053 - (-LiftingSurface.WingRegions(0).Length)))) Then
                '    score -= (LiftingSurface.WingRegions(0).Sweepback - rad2deg(Math.Atan((WingPosX - S * Math.Tan(phi)) / (-0.053 - (-LiftingSurface.WingRegions(0).Length))))) / (rad2deg(Math.Atan((WingPosX - S * Math.Tan(phi)) / (-0.053 - (-LiftingSurface.WingRegions(0).Length))))) * 100
                'End If

                Dim phi As Double = Math.Acos(L / 3)
                Dim MaxSpan As Double = Math.Tan(phi) * WingPosX
                If LiftingSurface.WingRegions(0).Length > MaxSpan Then
                    score -= (LiftingSurface.WingRegions(0).Length - MaxSpan) / MaxSpan * 100
                End If

            End If
        Next

    End Sub


#Region "SERVER"
    Public Sub CalculateOnServer()

        ' Connect to the server squekear to publish the messages and know when the calculation is over

        Dim Receiver As New UdpClient
        Receiver.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, True)
        Receiver.Client.Bind(New IPEndPoint(IPAddress.Any, 11001))

        ' Set up the squeaker

        Dim Squeaker As New UdpClient
        Squeaker.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, True)
        Squeaker.Connect("localhost", 11000)

        ' Request the calculation and start listening

        Select Case MyProjectRoot.SimulationSettings.AnalysisType
            Case CalculationType.SteadyState
                Squeak(Squeaker, "steady;" & FilePath)
            Case CalculationType.FreeFlight
                Squeak(Squeaker, "free_flight;" & FilePath)
            Case CalculationType.Aeroelastic
                Squeak(Squeaker, "aeroelastic;" & FilePath)
        End Select

        Dim Done As Boolean = False

        While Not Done

            Dim ClientAddress As New IPEndPoint(IPAddress.Any, 11001)
            Dim Message As String = Encoding.ASCII.GetString(Receiver.Receive(ClientAddress))

            Dim Commands As String() = Message.Split({";"c}, StringSplitOptions.RemoveEmptyEntries)

            If Commands.Count > 0 Then

                Select Case Commands(0)

                    Case "done"

                        Done = True
                        If Commands.Count > 1 Then
                            Dim DirectoryPath As String = Commands(1)
                            If IO.Directory.Exists(DirectoryPath) Then
                                MyProjectRoot.Results.LoadFromDirectory(DirectoryPath)
                            End If
                        End If

                End Select

            End If

        End While

        Squeaker.Close()
        Receiver.Close()
        ''''''''
        'RaiseEvent CalculationDone()
        ''''''''
    End Sub

    Private Sub Squeak(Squeaker As UdpClient, Message As String)
        Dim Bytes As Byte() = Text.Encoding.ASCII.GetBytes(Message)
        Squeaker.Send(Bytes, Bytes.Count)
    End Sub
#End Region

End Module
