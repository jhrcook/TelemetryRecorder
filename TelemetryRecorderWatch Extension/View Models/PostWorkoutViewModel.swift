//
//  PostWorkoutViewModel.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation

extension PostWorkoutView {
    func saveAFile() {
        dataManager.saveDataToFile()
        print("saved to: \(dataManager.saveFileURL)")
        phoneSyncIsComplete = true
    }
}
