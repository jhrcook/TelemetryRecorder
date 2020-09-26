//
//  TelemetryDataManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation
import CoreMotion

class TelemetryDataManager: ObservableObject {
    var hardwareData = HardwareData()
    
    var numberOfHardwareDatapoints: Int {
        get {
            hardwareData.pitch.count
        }
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
}
