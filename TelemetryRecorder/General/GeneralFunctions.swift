//
//  GeneralFunctions.swift
//  TelemetryRecorder
//
//  Created by Joshua on 9/26/20.
//

import Foundation

func getDocumentsDirectory() -> URL {
    let paths = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)
    return paths[0]
}
