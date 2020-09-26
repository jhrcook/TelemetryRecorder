//
//  WorkoutChoices.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation

class WorkoutChoices {
    let workouts: [WorkoutInformation]
    
    init() {
        workouts = WorkoutChoices.createWorkouts()
    }
}


extension WorkoutChoices {
    /// Create workouts to use in the app.
    /// - Returns: A list of workouts.
    static func createWorkouts() -> [WorkoutInformation] {
        var w = [WorkoutInformation]()
        
        w.append(WorkoutInformation(id: 0, name: "Push-Ups", type: .count, duration: 10))
        w.append(WorkoutInformation(id: 1, name: "Bicep Curls", type: .count, duration: 10))
        w.append(WorkoutInformation(id: 2, name: "Lunge", type: .count, duration: 10))
        w.append(WorkoutInformation(id: 3, name: "Squat", type: .count, duration: 15))
        
        return w
    }
}
