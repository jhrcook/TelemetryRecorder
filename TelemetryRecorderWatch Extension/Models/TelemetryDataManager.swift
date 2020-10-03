//
//  TelemetryDataManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation
import CoreMotion


struct TelemetryDataPoint: Codable {
    let date: Date
    let pitch: Double
    let yaw: Double
    let roll: Double
    let accelX: Double
    let accelY: Double
    let accelZ: Double
}


class TelemetryDataManager: ObservableObject {
    
    var workoutInfo: WorkoutInformation? = nil
    var telemetryData = [TelemetryDataPoint]()
    
    var numberOfTelemetryDataPoints: Int {
        get {
            telemetryData.count
        }
    }
    
    
    func updateMotionData(data: CMDeviceMotion, at date: Date) {
        let attitude: CMAttitude = data.attitude
        let acceleration: CMAcceleration = data.userAcceleration
        let dataPoint = TelemetryDataPoint(
            date: date,
            pitch: attitude.pitch, yaw: attitude.yaw, roll: attitude.roll,
            accelX: acceleration.x, accelY: acceleration.y, accelZ: acceleration.z
        )
        addDataPoint(dataPoint)
    }
    
    
    func reset() {
        telemetryData = []
        workoutInfo = nil
    }
    
    
    func addDataPoint(_ newDataPoint: TelemetryDataPoint) {
        telemetryData.append(newDataPoint)
    }
}
