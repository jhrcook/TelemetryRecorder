//
//  PostWorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI


struct PostWorkoutView: View {
    
    var dataSaver: DataSaver
    var watchCommunicator: WatchConnectivityManager
    
    @Environment(\.presentationMode) var presentationMode
    @State var saveFileStatus: TaskStatus = .incomplete
    @State var transferFileStatus: TaskStatus = .incomplete
    
    @State private var checkCancel = false
    
    var body: some View {
        VStack {
            
            Spacer()
            
            TaskCheckView(text: "Saved file", status: $saveFileStatus)
            TaskCheckView(text: "Transfered file", status: $transferFileStatus)
            
            Spacer()
            
            Button(action: {
                saveAndSyncData()
                DispatchQueue.global(qos: .userInitiated).async {
                    while saveFileStatus != .complete && transferFileStatus != .complete {
                        
                    }
                    DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                        presentationMode.wrappedValue.dismiss()
                    }
                }
            }) {
                Text("Save data")
            }
            
            Button(action: {
                cancelOperations()
                presentationMode.wrappedValue.dismiss()
            }, label: {
                Text("Cancel")
            })
        }
        .navigationBarBackButtonHidden(true)
    }
}


struct PostWorkoutView_Previews: PreviewProvider {
    static var previews: some View {
        PostWorkoutView(dataSaver: DataSaver(), watchCommunicator: WatchConnectivityManager())
    }
}
