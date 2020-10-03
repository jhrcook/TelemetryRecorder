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
    
    var date = Date()
    
    var dateFormatter: DateFormatter {
        let df = DateFormatter()
        df.dateFormat = "yyyy-MM-dd_HH-mm-ss"
        return df
    }
    
    var formattedDate: String {
        dateFormatter.string(from: date)
    }
    
    var saveFileName: String {
        let name: String = workoutInfo?.name ?? "unknown"
        let duration: String = workoutInfo == nil ? "unknown" : String(workoutInfo!.duration)
        return "workout-data_\(name)_\(duration)_\(formattedDate).json"
    }
    
    var saveFileURL: URL {
        getDocumentsDirectory().appendingPathComponent(saveFileName)
    }
    
    
    func updateMotionData(data: CMDeviceMotion) {
        let attitude: CMAttitude = data.attitude
        let acceleration: CMAcceleration = data.userAcceleration
        let dataPoint = HardwareDataPoint(
            pitch: attitude.pitch, yaw: attitude.yaw, roll: attitude.roll,
            accelX: acceleration.x, accelY: acceleration.y, accelZ: acceleration.z
        )
        hardwareData.data.append(dataPoint)
    }
    
    
    func saveDataToFile() {
        let data = hardwareData.dataAsDictionary()
        let encoder = JSONEncoder()
        
        do {
            let jsonData = try encoder.encode(data)
            try jsonData.write(to: saveFileURL)
        } catch {
            print("erorr encoding/writing data: \(error.localizedDescription)")
        }
    }
    
    
    func reset() {
        hardwareData.data = []
        workoutInfo = nil
        date = Date()
    }
}



class HardwareData {
    
    var data = [HardwareDataPoint]()
    
    func dataAsDictionary() -> [String: [String: [Double]]] {
        return [
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
    var pitch: Double
    var yaw: Double
    var roll: Double
    var accelX: Double
    var accelY: Double
    var accelZ: Double
}
