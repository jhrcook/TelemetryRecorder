//
//  PostWorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI


struct PostWorkoutView: View {
    
    var dataManager: TelemetryDataManager
    
    @State var phoneSyncIsComplete = false

    
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


struct PostWorkoutView_Previews: PreviewProvider {
    static var previews: some View {
        PostWorkoutView(dataManager: TelemetryDataManager())
    }
}
