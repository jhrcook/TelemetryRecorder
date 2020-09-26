//
//  WorkoutManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation

class WorkoutManager: NSObject, ObservableObject {
    var info: WorkoutInformation
    
    init(info: WorkoutInformation) {
        self.info = info
    }
}
