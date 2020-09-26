//
//  WorkoutDurationSelectionView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct WorkoutDurationSelectionView: View {
    
    @Binding var workoutInformation: WorkoutInformation?
    @State private var selectedDuration: Int = 0
    
    @Environment(\.presentationMode) var presentationMode
    
    var body: some View {
        VStack {
            
            Text("Workout duration")
            
            Picker("Workout duration", selection: $selectedDuration) {
                ForEach(0..<121) { i in
                    Text("\(i)").font(.system(size: 26))
                }
            }
            .labelsHidden()
            
            Button(action: {
                workoutInformation?.duration = selectedDuration
                presentationMode.wrappedValue.dismiss()
            }) {
                Text("Okay")
            }
        }
        .onAppear {
            selectedDuration = workoutInformation?.duration ?? 0
        }
    }
}

struct WorkoutDurationSelectionView_Previews: PreviewProvider {
    
    static let workoutChoices = WorkoutChoices()
    
    static var previews: some View {
        WorkoutDurationSelectionView(workoutInformation: .constant(workoutChoices.workouts[0]))
    }
}
