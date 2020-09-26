//
//  WorkoutListView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct WorkoutListView: View {
    
    let workoutChoices = WorkoutChoices()
    
    var body: some View {
        List {
            ForEach(workoutChoices.workouts) { workoutInfo in
                NavigationLink(
                    destination: WorkoutDurationSelectionView(workoutInformation: workoutInfo),
                    label: {
                        HStack {
                            Text(workoutInfo.name)
                            Spacer()
                            Image(systemName: "chevron.right").opacity(0.5)
                        }
                    })
            }
        }
    }
}

struct WorkoutListView_Previews: PreviewProvider {
    static var previews: some View {
        WorkoutListView()
    }
}
