//
//  DataSaver.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 10/3/20.
//

import Foundation

struct DataSaver {
    
    var workoutInfo: WorkoutInformation? = nil
    var telemetryData: TelemetryDataManager? = nil
    var workoutData: WorkoutManager? = nil
    
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
    
    
    struct CombinedData: Codable {
        let telemetryData: [TelemetryDataPoint]?
        let workoutData: [WorkoutDataPoint]?
    }
    
    
    func saveDataToFile() {
        let data = CombinedData(telemetryData: telemetryData?.telemetryData, workoutData: workoutData?.workoutData)
        let encoder = JSONEncoder()
        
        do {
            let jsonData = try encoder.encode(data)
            try jsonData.write(to: saveFileURL)
        } catch {
            print("error encoding/writing data: \(error.localizedDescription)")
        }
    }
}
