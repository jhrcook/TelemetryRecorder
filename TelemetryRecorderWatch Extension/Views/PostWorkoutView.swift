//
//  PostWorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI


struct PostWorkoutView: View {
    
    var dataManager: TelemetryDataManager
    
    @State private var phoneSyncIsComplete = false

    
    var body: some View {
        ZStack {
            VStack {
                Text("Syncing with iCloud...")
                
                Button(action: {
                    saveAFile()
                }, label: {
                    Text("Save a file!")
                })
            }
            .navigationBarBackButtonHidden(true)
            
            NavigationLink(
                destination: ContentView(),
                isActive: $phoneSyncIsComplete) {
                EmptyView()
            }
            .opacity(0)
        }
    }
}


extension PostWorkoutView {
    func saveAFile() {
        // TODO: Link with iPhone and send data to then be saved to iCloud.
        phoneSyncIsComplete = true
    }
}

struct PostWorkoutView_Previews: PreviewProvider {
    static var previews: some View {
        PostWorkoutView(dataManager: TelemetryDataManager())
    }
}
