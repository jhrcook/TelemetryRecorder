//
//  TelemetryDataManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation
import CoreMotion

class TelemetryDataManager: ObservableObject {
    
    var workoutInfo: WorkoutInformation? = nil
    var hardwareData = HardwareData()
    
    var numberOfHardwareDataPoints: Int {
        get {
            hardwareData.data.count
        }
    }
    
    func updateMotionData(data: CMDeviceMotion, at date: Date) {
        let attitude: CMAttitude = data.attitude
        let acceleration: CMAcceleration = data.userAcceleration
        let dataPoint = HardwareDataPoint(
            date: date,
            pitch: attitude.pitch, yaw: attitude.yaw, roll: attitude.roll,
            accelX: acceleration.x, accelY: acceleration.y, accelZ: acceleration.z
        )
        hardwareData.data.append(dataPoint)
    }
    
    func reset() {
        hardwareData.data = []
        workoutInfo = nil
    }
}



class HardwareData {
    
    var data = [HardwareDataPoint]()
    
    var dateFormatter: DateFormatter {
        let df = DateFormatter()
        df.dateFormat = "yyyy-MM-dd_HH:mm:ss"
        return df
    }
    
    func dataAsDictionary() -> [String: [String: [Double]]] {
        return [
            "other": [
                "date": data.map { $0.date.timeIntervalSince1970 }
            ],
            "attitude": [
                "pitch": data.map({ $0.pitch }),
                "yaw": data.map({ $0.yaw }),
                "roll": data.map({ $0.roll })
            ],
            "acceleration": [
                "x": data.map({ $0.accelX }),
                "y": data.map({ $0.accelY }),
                "z": data.map({ $0.accelZ })
            ]
        ]
    }
}


struct HardwareDataPoint {
    let date: Date
    let pitch: Double
    let yaw: Double
    let roll: Double
    let accelX: Double
    let accelY: Double
    let accelZ: Double
}
