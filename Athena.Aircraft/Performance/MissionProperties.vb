
Namespace Performance
    Partial Public Class Mission
        Public Class MissionProperties

            ''timer
            Public Property TimeTotal As Double = 0
            Public Property TimeRotate As Double = 0
            Public Property TimeLiftOff As Double = 0
            Public Property TimeCeiling As Double = 0
            Public Property TimeConstantEnergy As Double = 0
            Public Property TimeDescentCruise As Double = 0

            ''distance
            Public Property DistanceTotal As Double = 0
            Public Property DistanceRotate As Double = 0
            Public Property DistanceLiftOff As Double = 0
            Public Property DistanceCeiling As Double = 0
            Public Property DistanceConstantEnergy As Double = 0
            Public Property DistanceDescentCruise As Double = 0


            ''velocity
            Public Property VelocityRotate As Double = 0
            Public Property VelocityLiftOff As Double = 0
            Public Property VelocityCeiling As Double = 0
            Public Property VelocityFinalConstantEnergy As Double = 0

            Public Property V_MAX As Double = 0

            'altitude
            Public Property AltitudeCeiling As Double = 0
            Public Property AltitudeFinalConstantEnergy As Double = 0

            Public Property EnergyAltitudeCeiling As Double = 0 ''He at top of climb | kept const until optimum DESC point is met

            ''scoring
            Public Property TakeOffBonus As Double = 0
            Public Property PreScore_Altitude_Team As Double = 0
            Public Property AltitudeScore As Double = 0
            Public Property DistanceScore As Double = 0


            Public Property TakeOffWeight As Double
            Public Property Drag As Double() = New Double() {}
            Public Property Lift As Double() = New Double() {}
            Public Property AOA_GE As Double() = New Double() {}
            Public Property Altitude_GE As Double() = New Double() {}
            Public Property Drag_GE As Double() = New Double() {}
            Public Property Lift_GE As Double() = New Double() {}
            Public Property Reynolds As Double() = New Double() {}
            Public Property AoA As Double() = New Double() {}
            Public Property Elevator_Deflection As Double() = New Double() {}


            ''polar coefficients
            Public Property Cd0 As Double
            Public Property k As Double
            Public Property b As Double
        End Class

    End Class

End Namespace

