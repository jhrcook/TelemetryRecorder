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
    
    /// - Tag: Data
    var workoutInfo: WorkoutInformation? = nil
    var telemetryData = [TelemetryDataPoint]()
    
    var numberOfTelemetryDataPoints: Int {
        get {
            telemetryData.count
        }
    }
    
    
    /// Update the motion data.
    /// - Parameters:
    ///   - data: Device motion data from `CoreMotion`.
    ///   - date: When the data was collected.
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
    
    
    /// Reset the data.
    ///
    /// Sets the workout info to `nil` and data array is emptied.
    func reset() {
        telemetryData = []
        workoutInfo = nil
    }
    
    
    /// Add a data point to the telemetry data array.
    /// - Parameter newDataPoint: The new data point.
    func addDataPoint(_ newDataPoint: TelemetryDataPoint) {
        telemetryData.append(newDataPoint)
    }
}
