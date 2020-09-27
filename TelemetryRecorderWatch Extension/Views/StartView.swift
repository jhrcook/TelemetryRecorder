//
//  StartView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

//struct StartView: View {
//
//    @ObservedObject var workoutManager: WorkoutManager
//
//    var body: some View {
//        VStack {
//            Text(workoutManager.info.name)
//                .font(.title)
//                .padding(.vertical, 5)
//
//            Text("\(workoutManager.info.duration) \(workoutManager.info.type == .count ? "reps" : "sec.")")
//                .font(.headline)
//                .padding(.bottom, 5)
//
//            Spacer()
//
//            NavigationLink(destination: WorkoutView(workoutManager: workoutManager)) {
//                Text("Start")
//                    .font(.title)
//                    .foregroundColor(.green)
//                    .padding()
//            }
//        }
//    }
//}
//
//struct StartView_Previews: PreviewProvider {
//
//    static let workoutChoices = WorkoutChoices()
//
//    static var previews: some View {
//        StartView(workoutManager: WorkoutManager(info: workoutChoices.workouts[0]))
//    }
//}
