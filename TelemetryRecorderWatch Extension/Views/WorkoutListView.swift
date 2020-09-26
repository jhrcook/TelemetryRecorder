//
//  WorkoutListView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct WorkoutListView: View {
    
    let workoutChoices = WorkoutChoices()
    @Binding var workoutSelected: WorkoutInformation?
    
    @Environment(\.presentationMode) var presentationMode
    
    var body: some View {
        List {
            ForEach(workoutChoices.workouts) { workoutInfo in
                Button(action: {
                    userSelected(workout: workoutInfo)
                }, label: {
                    Text(workoutInfo.name)
                })
            }
        }
    }
    
    func userSelected(workout: WorkoutInformation) {
        workoutSelected = workout
        presentationMode.wrappedValue.dismiss()
    }
}

struct WorkoutListView_Previews: PreviewProvider {
    static var previews: some View {
        WorkoutListView(workoutSelected: .constant(nil))
    }
}
