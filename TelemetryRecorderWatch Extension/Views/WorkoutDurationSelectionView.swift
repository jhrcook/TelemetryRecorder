//
//  WorkoutDurationSelectionView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct WorkoutDurationSelectionView: View {
    
    init(workoutInformation: WorkoutInformation) {
        self.workoutManager = WorkoutManager(info: workoutInformation)
    }
    
    @ObservedObject var workoutManager: WorkoutManager
    @State private var selectedDuration: Int = 0
    @State private var startWorkout = false
    
    var body: some View {
        VStack {
            
            Text("Workout duration")
            
            Picker("Workout duration", selection: $selectedDuration) {
                ForEach(0..<121) { i in
                    Text("\(i)").font(.system(size: 26))
                }
            }
            .labelsHidden()
            
            NavigationLink(
                destination: StartView(workoutManager: self.workoutManager)) {
                Text("OK")
                    .foregroundColor(.white)
                    .font(.headline)
            }
        }
        .onAppear {
            self.selectedDuration = workoutManager.info.duration
        }
    }
}

struct WorkoutDurationSelectionView_Previews: PreviewProvider {
    
    static let workoutChoices = WorkoutChoices()
    
    static var previews: some View {
        WorkoutDurationSelectionView(workoutInformation: workoutChoices.workouts[0])
    }
}
