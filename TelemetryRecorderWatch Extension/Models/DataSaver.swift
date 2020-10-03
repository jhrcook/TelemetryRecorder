//
//  DataSaver.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 10/3/20.
//

import Foundation

struct DataSaver {
    
    var workoutInfo: WorkoutInformation? = nil
    var telemetryData: HardwareData? = nil
    var workoutData: WorkoutData? = nil
    
    var date = Date()
    
    var dateFormatter: DateFormatter {
        let df = DateFormatter()
        df.dateFormat = "yyyy-MM-dd_HH-mm-ss"
        return df
    }
    
    var formattedDate: String {
        dateFormatter.string(from: date)
    }
    
    var saveFileName: String {
        let name: String = workoutInfo?.name ?? "unknown"
        let duration: String = workoutInfo == nil ? "unknown" : String(workoutInfo!.duration)
        return "workout-data_\(name)_\(duration)_\(formattedDate).json"
    }
    
    var saveFileURL: URL {
        getDocumentsDirectory().appendingPathComponent(saveFileName)
    }
    
    func saveDataToFile() {
        let data: [String: Encodable] = [
            "telemetryData": telemetryData?.dataAsDictionary() ?? "nil",
            "workoutData": workoutData?.dataAsDictionary() ?? "nil"
        ]
        let encoder = JSONEncoder()
        
        do {
            let jsonData = try encoder.encode(data)
            try jsonData.write(to: saveFileURL)
        } catch {
            print("error encoding/writing data: \(error.localizedDescription)")
        }
    }
}
