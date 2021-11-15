Imports OpenVOGEL.AeroTools.CalculationModel.Settings
Imports Athena.Functions.Functions
Imports Athena.Aircraft.Models.Design
Imports Athena.Aircraft.Performance.Mission.MissionProperties


Namespace Performance

    Partial Public Class Mission
        Public Property Vminimun_unstick As Double
        Public Property Vrotate As Double
        Public Property Vinst As Double
        Public Property Sto As Double
        Public Property Accel As Double
        Public Property AOA_unstick As Double
        Public Property μ As Double 'ground friction coefficient

        Public Sub Initialize(ByRef Properties As MissionProperties, ByVal PolarNo As Double, ByVal Vel As Double, Density As Double)

            Structural.Weight(WingArea, RudderArea, ElevatorArea, FuselageArea, Properties)

            Dim Re, Lift_Inst, Cl As Double
            Dim ISA As New StandardAtmosphere(0)
            ReDim Properties.Drag(Loads.Count - 1)
            ReDim Properties.Lift(Loads.Count - 1)
            ReDim Properties.Reynolds(Loads.Count - 1)
            ReDim Properties.AoA(Loads.Count - 1)
            ReDim Properties.Elevator_Deflection(Loads.Count - 1)
            Dim q As Double = 0.5 * Density * (WingArea / 2) * (Vel ^ 2)
            Dim i, n As Integer
            i = 0
            n = 0
            Do Until i > Loads.Count - 1
                If Loads(i).Drag < 0 OrElse Loads(i).Lift < 0 Then
                    ReDim Preserve Properties.Drag(Properties.Drag.GetUpperBound(0) - 1)
                    ReDim Preserve Properties.Lift(Properties.Lift.GetUpperBound(0) - 1)
                    ReDim Preserve Properties.Reynolds(Properties.Reynolds.GetUpperBound(0) - 1)
                    ReDim Preserve Properties.AoA(Properties.AoA.GetUpperBound(0) - 1)
                    ReDim Properties.Elevator_Deflection(Properties.Elevator_Deflection.GetUpperBound(0) - 1)
                    i += 1
                    Continue Do
                End If
                Properties.Drag(n) = Loads(i).Drag / q
                Properties.Lift(n) = Loads(i).Lift / q
                Properties.Reynolds(n) = Loads(i).Re
                Properties.AoA(n) = Math.Round(Loads(i).AoA, 1)
                Properties.Elevator_Deflection(n) = Loads(i).Deflection
                n += 1
                i += 1
            Loop
            If Properties.AoA.Length < 5 Then
                Throw New ArgumentOutOfRangeException
            End If
            Select Case PolarNo
                Case 1
                    Dim Polyonym = polyfit2(Properties.Lift, Properties.Drag)
                    Properties.Cd0 = Polyonym.c1
                    Properties.k = Polyonym.a1
                    Properties.b = Polyonym.b1

                Case Else
                    Dim Dummy(Properties.AoA.GetUpperBound(0)) As Double
                    Properties.AoA.CopyTo(Dummy, 0)
                    Array.Sort(Dummy)
                    Dim NewDrag As Double() = New Double() {}
                    Dim NewLift As Double() = New Double() {}
                    Dim NewRe As Double() = New Double() {}
                    Dim NewAoa As Double() = New Double() {}
                    Dim count As Integer = 0
                    Dim j As Integer = 0
                    While j <= Dummy.GetUpperBound(0) - PolarNo + 1
                        Dim sum As New Double
                        For y = j To j + PolarNo - 1
                            sum += Dummy(y)
                        Next
                        Dim mean As Double = sum / PolarNo
                        If mean = Dummy(j) Then
                            For y = 0 To Dummy.Length - 1
                                If Properties.AoA(y) = mean Then
                                    ReDim Preserve NewDrag(count)
                                    ReDim Preserve NewLift(count)
                                    ReDim Preserve NewRe(count)
                                    ReDim Preserve NewAoa(count)
                                    NewDrag(count) = Properties.Drag(y)
                                    NewLift(count) = Properties.Lift(y)
                                    NewRe(count) = Properties.Reynolds(y)
                                    NewAoa(count) = Properties.AoA(y)
                                    count += 1
                                End If
                            Next
                            j += PolarNo
                        Else
                            j += 1
                        End If
                    End While
                    ReDim Properties.Drag(count - 1)
                    ReDim Properties.Lift(count - 1)
                    ReDim Properties.Reynolds(count - 1)
                    ReDim Properties.AoA(count - 1)
                    NewDrag.CopyTo(Properties.Drag, 0)
                    NewLift.CopyTo(Properties.Lift, 0)
                    NewRe.CopyTo(Properties.Reynolds, 0)
                    NewAoa.CopyTo(Properties.AoA, 0)

            End Select


            AOA_unstick = 10 'rad2deg(Math.Atan(0.15 / (0.3 * Fuselage_length))) 'it is supposed that Main LG is position at 0.6*Fus_Length and we gine an exgtra 10% margin
            '0.15 is the height of LG
            Cl = interp1d(Properties.AoA, Properties.Lift, AOA_unstick)
            Vminimun_unstick = Math.Sqrt(Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Cl))
            Vrotate = Vminimun_unstick * 1.1

            Vinst = 0
            μ = 0.06
            Accel = 0

        End Sub

        Public Sub TakeOff(ByRef Properties As MissionProperties)
            '
            'NEEDS FIXENG FOR WHEN L>W BUT V CANT REACH VROTATE (AMEND V ROTATE BY 1.1VSTAL)
            '
            Dim ISA As New StandardAtmosphere(0)
            Dim Drag_Inst As Double
            Dim dt As Double = 0.01 'time steps for the calculation
            Dim TakeOff_Distance As Double
            Dim Timestep As Double = 0
            Dim Lift_Inst, Cl, Cd As Double
            Dim AOA As Double = 0
            While True 'Vrotate >= Vinst

                ''''
                Dim Re As Double = Vinst * Lmac / ISA.KinematicVisc
                'Drag_Inst = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, AOA)
                'Lift_Inst = interp2d(Properties.Reynolds, Properties.AoA, Properties.Lift, Re, AOA)

                Cl = interp1d(Properties.AoA, Properties.Lift, AOA)
                Lift_Inst = 0.5 * ISA.Density * (WingArea / 2) * Cl * Vinst ^ 2

                'If Lift_Inst > Properties.TakeOffWeight Then
                '    Exit While
                'End If

                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Drag_Inst = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vinst ^ 2
                ''''

                Accel = (Engine.Thrust(Vinst) - μ * (Lift_Inst - Properties.TakeOffWeight) - Drag_Inst) / (Properties.TakeOffWeight / 9.81)
                If 1.39 <= Vinst Then ''AndAlso Enter Then ''at 1.39m/s (5km/h) the timing starts 
                    Timestep += 1
                End If

                ''next timestep status''
                TakeOff_Distance += Vinst * dt + 0.5 * Accel * dt ^ 2
                If TakeOff_Distance > 60 OrElse Timestep * dt > 60 Then
                    Throw New ArgumentOutOfRangeException
                End If
                Vinst += Accel * dt
                If Vinst >= Vrotate Then
                    Exit While
                End If
            End While
            Properties.TimeRotate = Timestep * dt

            If TakeOff_Distance <= 40 Then
                Properties.TakeOffBonus = 1.1
            ElseIf TakeOff_Distance <= 60 Then
                Properties.TakeOffBonus = 1
            End If
            Properties.DistanceTotal = TakeOff_Distance
            Properties.DistanceRotate = TakeOff_Distance

            Properties.VelocityRotate = Vinst
        End Sub

        Public Sub Climb(ByRef Properties As MissionProperties)
            Dim ISA As New StandardAtmosphere
            Dim dHe As Integer = 1
            Dim Alt As Double = 0
            Dim dt As Double
            ISA.CalculateAirProperties(Alt)
            Dim Vel, Vel1, Re, Alpha, Thrust_req, Thrust_max, SEP1, dV, SEP2, Gradient, Cl, Cd, V_old As Double
            Vel = Properties.VelocityRotate

            Dim He As Double
            dV = 0.1
            ''find optimum Vel to start climbing (Sep=0)
            While True
                ''''
                'Re = Vel * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                ''''''
                Thrust_max = Engine.Thrust(Vel)
                Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                SEP1 = (Thrust_max - Thrust_req) * Vel / Properties.TakeOffWeight
                Vel1 = Vel + dV
                ''''
                'Re = Vel1 * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel1 ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel1 ^ 2
                '''''''''
                Thrust_max = Engine.Thrust(Vel1)
                SEP2 = (Thrust_max - Thrust_req) * Vel1 / Properties.TakeOffWeight
                If (SEP2 - SEP1) < 0 Then
                    dV /= 10
                    Continue While
                End If
                Gradient = (SEP2 - SEP1) / dV
                dt = dV / Accel

                Properties.TimeLiftOff += dt
                Properties.DistanceLiftOff += (Vel) * dt + 0.5 * Accel * dt ^ 2
                If Math.Abs(Gradient) < 10 ^ (-4) Then
                    Exit While
                End If

                Vel = Vel1 + dV
            End While
            Properties.VelocityLiftOff = Vel

            He = Alt + Vel ^ 2 / (2 * 9.81)
            V_old = Vel
            Properties.TimeCeiling += dHe / SEP1
            'Properties.DistanceCeiling += (Vel1 + dV) * (dHe / SEP1) + 0.5 * Accel * (dHe / SEP1) ^ 2
            He += dHe
            dV = 0.1
            While True
                Alt = He - Vel ^ 2 / (2 * 9.81)
                While True
                    ISA.CalculateAirProperties(Alt)
                    ''''
                    'Re = Vel * Lmac / ISA.KinematicVisc
                    'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                    'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                    Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                    Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                    Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                    ''''
                    Thrust_max = Engine.Thrust(Vel)
                    Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                    SEP1 = (Thrust_max - Thrust_req) * Vel / Properties.TakeOffWeight
                    Vel1 = Vel + dV
                    Alt = He - Vel1 ^ 2 / (2 * 9.81)
                    ISA.CalculateAirProperties(Alt)
                    ''''
                    'Re = Vel1 * Lmac / ISA.KinematicVisc
                    'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                    'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                    Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel1 ^ 2)
                    Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                    Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel1 ^ 2
                    ''''
                    Thrust_max = Engine.Thrust(Vel1)
                    SEP2 = (Thrust_max - Thrust_req) * Vel1 / Properties.TakeOffWeight
                    If (SEP2 - SEP1) < 0 Then
                        dV /= 10
                        Continue While
                    End If
                    Gradient = (SEP2 - SEP1) / dV
                    If Math.Abs(Gradient) < 10 ^ (-4) Then
                        Exit While
                    End If

                    Vel = Vel1 + dV
                    Alt = He - Vel ^ 2 / (2 * 9.81)
                End While

                ''current altitude
                Alt = He - Vel ^ 2 / (2 * 9.81)

                '' distance covered
                Properties.DistanceCeiling += ((Vel + V_old) / 2) * (dHe / SEP1)
                V_old = Vel
                If Alt >= 107.5 OrElse (Properties.TimeRotate + Properties.TimeLiftOff + Properties.TimeCeiling) >= 60 Then
                    Exit While
                ElseIf Double.IsInfinity(Vel) OrElse Thrust_req < 0 Then
                    Throw New ArgumentOutOfRangeException
                End If

                ''time to go from Hei to Hei+1///if timer exceeds 60sec then climb is terminated and total (time = Timer.Climb -= dHe / SEP1)

                Properties.TimeCeiling += dHe / SEP1

                '' next energy level
                He += dHe
                dV = 0.1
                Vel = Properties.VelocityLiftOff / 2
            End While

            Properties.AltitudeCeiling = Alt
            Properties.VelocityCeiling = Vel
            Properties.EnergyAltitudeCeiling = He

            Properties.PreScore_Altitude_Team = (-3.92 * 10 ^ (-5)) * (Alt ^ 4) + (1.08 * 10 ^ (-2)) * (Alt ^ 3) + (-1.156) * (Alt ^ 2) + 64.2 * Alt - 537
            Properties.AltitudeScore = Properties.PreScore_Altitude_Team

        End Sub

        Public Sub ConstantEnergyCruise(ByRef Properties As MissionProperties)
            Dim ISA As New StandardAtmosphere
            Dim Alt, maxalt, maxM As Double
            Dim dt As Double
            Dim He, Vel, Vel1, Re, Alpha, Thrust_req, Thrust_max, SEP1, SEP2, ExT, dV, Gradient, DESC1, DESC2, Cl, Cd As Double

            ''find Global MAX Velocity
            For Alt = 0 To 130 Step 0.5
                For Mach = 0 To 0.09 Step 0.001
                    ISA.CalculateAirProperties(Alt)
                    Vel = Mach * ISA.SoundSpeed
                    ''''
                    'Re = Vel * Lmac / ISA.KinematicVisc
                    'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                    'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                    Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                    Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                    Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                    ''''
                    Thrust_max = Engine.Thrust(Vel)
                    If Thrust_max - Thrust_req < 0 Then
                        Continue For
                    End If
                    If Vel > Properties.V_MAX Then
                        Properties.V_MAX = Vel
                        maxalt = Alt
                        maxM = Mach
                    End If
                Next
            Next
            If Properties.V_MAX = 0 Then
                Throw New ArgumentOutOfRangeException
            End If
            ''find optimum cruise point =>Vmax | He=const =>T=D | SEP=0

            Vel = Properties.VelocityCeiling

            He = Properties.EnergyAltitudeCeiling
            dV = 0.1
            While True
                Alt = He - Vel ^ 2 / (2 * 9.81)
                ISA.CalculateAirProperties(Alt)
                ''''
                'Re = Vel * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                ''''
                Thrust_max = Engine.Thrust(Vel)
                Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                ExT = Thrust_max - Thrust_req
                If Thrust_max < Thrust_req Then
                    Vel -= dV
                    dV /= 10
                    Vel += dV
                    Continue While
                End If
                If ExT < 10 ^ (-3) Then
                    Exit While
                ElseIf Double.IsInfinity(Vel) OrElse Thrust_req < 0 Then
                    Throw New ArgumentOutOfRangeException
                End If

                Vel += dV

            End While

            ''enter SEP<0 | V>Vmax region
            dV = 1 - (Vel / Properties.V_MAX)

            While True
                Alt = He - Vel ^ 2 / (2 * 9.81)
                ISA.CalculateAirProperties(Alt)
                ''''
                'Re = Vel * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                ''''
                Thrust_max = Engine.Thrust(Vel)
                Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)

                If Vel - Properties.V_MAX > 0.2 * Properties.V_MAX Then
                    Exit While
                ElseIf Double.IsInfinity(Vel) OrElse Thrust_req < 0 Then
                    Throw New ArgumentOutOfRangeException
                End If

                'dt = Math.Abs(dV / Accel)
                'Properties.TimeConstantEnergy += dt
                'Properties.DistanceConstantEnergy += Vel * dt + 0.5 * Accel * dt ^ 2
                If (Properties.V_MAX - Vel) >= 0.1 Then
                    dV = 1 - (Vel / Properties.V_MAX)
                    Vel += dV
                Else
                    dV = Math.Abs(1 - (Vel + 0.1) / Properties.V_MAX)
                    Vel += dV
                End If
            End While

            ''find optimum descent rate starting point

            dV = 0.1
            Dim r1, r2 As Double
            While True
                Alt = He - Vel ^ 2 / (2 * 9.81)
                ISA.CalculateAirProperties(Alt)
                ''''
                'Re = Vel * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                ''''
                Thrust_max = Engine.Thrust(Vel)
                Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                SEP1 = (Thrust_max - Thrust_req) * Vel / Properties.TakeOffWeight
                DESC1 = (Vel - Properties.V_MAX) / SEP1
                r1 = DESC1 / SEP1
                Vel1 = Vel + dV
                Alt = He - Vel1 ^ 2 / (2 * 9.81)
                ISA.CalculateAirProperties(Alt)
                ''''
                'Re = Vel1 * Lmac / ISA.KinematicVisc
                'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel1 ^ 2)
                Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel1 ^ 2
                ''''
                Thrust_max = Engine.Thrust(Vel1)
                SEP2 = (Thrust_max - Thrust_req) * Vel1 / Properties.TakeOffWeight
                DESC2 = (Vel1 - Properties.V_MAX) / SEP2
                r2 = DESC2 / SEP2
                If (DESC2 - DESC1) > 0 Then
                    'If (r1 - r2) < 0 Then
                    dV /= 10
                    Continue While
                End If
                Gradient = (DESC2 - DESC1) / dV
                If Math.Abs(Gradient) < (10 ^ (-4)) Then
                    Exit While
                ElseIf Double.IsInfinity(Vel) OrElse Thrust_req < 0 Then
                    Throw New ArgumentOutOfRangeException
                End If

                dV = 0.1
                Vel = Vel1 + dV
            End While
            Alt = He - Vel ^ 2 / (2 * 9.81)
            Properties.VelocityFinalConstantEnergy = Vel
            Properties.AltitudeFinalConstantEnergy = Alt
        End Sub

        Public Sub DescentCruise(ByRef Properties As MissionProperties)
            Dim ISA As New StandardAtmosphere
            Dim Alt, dt, He, dHe, Vel, Vel1, Re, Alpha, Thrust_req, Thrust_max, SEP1, SEP2, dV, Gradient, DESC1, DESC2, dx, Vel_old, Cl, Cd As Double
            dHe = 1
            Dim r1, r2 As Double

            ''calculate dx,dt from He -> (He-dHe)
            ISA.CalculateAirProperties(Properties.AltitudeFinalConstantEnergy)
            'Re = Properties.VelocityFinalConstantEnergy * Lmac / ISA.KinematicVisc
            'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
            'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)
            Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Properties.VelocityFinalConstantEnergy ^ 2)
            Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
            Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Properties.VelocityFinalConstantEnergy ^ 2
            Thrust_max = Engine.Thrust(Properties.VelocityFinalConstantEnergy)
            SEP1 = (Thrust_max - Thrust_req) * Properties.VelocityFinalConstantEnergy / Properties.TakeOffWeight

            dt = Math.Abs(dHe / SEP1)
            Properties.TimeDescentCruise += dt
            Vel_old = Properties.VelocityFinalConstantEnergy
            Vel = Properties.V_MAX + 0.2 * Properties.V_MAX
            He = Properties.EnergyAltitudeCeiling - dHe
            dV = 0.1
            While True
                While True
                    Alt = He - Vel ^ 2 / (2 * 9.81)
                    ISA.CalculateAirProperties(Alt)
                    ''''
                    'Re = Properties.VelocityFinalConstantEnergy * Lmac / ISA.KinematicVisc
                    'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                    'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                    Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                    Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                    Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                    ''''
                    Thrust_max = Engine.Thrust(Vel)
                    Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                    SEP1 = (Thrust_max - Thrust_req) * Vel / Properties.TakeOffWeight
                    DESC1 = (Vel - Properties.V_MAX) / SEP1
                    r1 = DESC1 / SEP1
                    Vel1 = Vel + dV
                    Alt = He - Vel1 ^ 2 / (2 * 9.81)
                    ISA.CalculateAirProperties(Alt)
                    ''''
                    'Re = Vel1 * Lmac / ISA.KinematicVisc
                    'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                    'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                    Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel1 ^ 2)
                    Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                    Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel1 ^ 2
                    ''''
                    Thrust_max = Engine.Thrust(Vel1)
                    SEP2 = (Thrust_max - Thrust_req) * Vel1 / Properties.TakeOffWeight
                    DESC2 = (Vel1 - Properties.V_MAX) / SEP2
                    r2 = DESC2 / SEP2
                    If (DESC2 - DESC1) > 0 Then
                        dV /= 10
                        Continue While
                    End If
                    Gradient = (DESC2 - DESC1) / dV
                    If Math.Abs(Gradient) < 10 ^ (-4) Then
                        Exit While
                    End If
                    dV = 0.1
                    Vel = Vel1 + dV
                End While

                dx = ((Vel + Vel_old) / 2) * dt
                Properties.DistanceDescentCruise += dx
                dt = Math.Abs(dHe / SEP1)

                If Alt <= 15 Or (Properties.TimeDescentCruise + Properties.TimeConstantEnergy + dt) >= 120 Then
                    Exit While
                ElseIf Alt <= 18 AndAlso (Properties.TimeDescentCruise + Properties.TimeConstantEnergy + dt) < 120 Then
                    While True
                        dt = 0.01
                        Properties.DistanceDescentCruise += Vel * dt + 0.5 * Accel * dt ^ 2
                        Properties.TimeDescentCruise += dt

                        If Properties.TimeDescentCruise >= 120 Then
                            Exit While
                        End If

                        Vel += Accel * dt
                        ISA.CalculateAirProperties(Alt)
                        ''''
                        'Re = Vel * Lmac / ISA.KinematicVisc
                        'Alpha = interp2d(Properties.Reynolds, Properties.Lift, Properties.AoA, Re, Properties.TakeOffWeight)
                        'Thrust_req = interp2d(Properties.Reynolds, Properties.AoA, Properties.Drag, Re, Alpha)

                        Cl = Properties.TakeOffWeight / (0.5 * ISA.Density * (WingArea / 2) * Vel ^ 2)
                        Cd = Properties.Cd0 + Properties.b * Cl + Properties.k * Cl ^ 2
                        Thrust_req = 0.5 * ISA.Density * (WingArea / 2) * Cd * Vel ^ 2
                        ''''
                        Thrust_max = Engine.Thrust(Vel)
                        Accel = (Thrust_max - Thrust_req) / (Properties.TakeOffWeight / 9.81)
                    End While
                    Exit While

                ElseIf Double.IsInfinity(Vel) OrElse Thrust_req < 0 Then
                    Throw New ArgumentOutOfRangeException
                End If

                Properties.TimeDescentCruise += dt
                dV = 0.1
                Vel_old = Vel
                Vel = Properties.V_MAX + 0.2 * Properties.V_MAX
                He += -dHe
            End While

            Properties.DistanceScore = Properties.DistanceDescentCruise
        End Sub

    End Class

End Namespace
