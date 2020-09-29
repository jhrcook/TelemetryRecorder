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
    
    var numberOfHardwareDatapoints: Int {
        get {
            hardwareData.pitch.count
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
        hardwareData.pitch.append(attitude.pitch)
        hardwareData.yaw.append(attitude.yaw)
        hardwareData.roll.append(attitude.roll)
    }
    
    func updateAccelerationData(data: CMAccelerometerData) {
        let acceleration = data.acceleration
        hardwareData.accelX.append(acceleration.x)
        hardwareData.accelY.append(acceleration.y)
        hardwareData.accelZ.append(acceleration.z)
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
}



class HardwareData {
    // Attitude
    var pitch = [Double]()
    var yaw = [Double]()
    var roll = [Double]()
    
    // Acceleration
    var accelX = [Double]()
    var accelY = [Double]()
    var accelZ = [Double]()
    
    func dataAsDictionary() -> [String: [String: [Double]]] {
        return [
            "attitude": [
                "pitch": pitch,
                "yaw": yaw,
                "roll": roll
            ],
            "acceleration": [
                "x": accelX,
                "y": accelY,
                "z": accelZ
            ]
        ]
    }
}
