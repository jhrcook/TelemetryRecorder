//
//  WatchConnectivityManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation
import WatchConnectivity


protocol WatchTransferDelegate {
    func transferCompletedSuccessfully()
    func transferCompletedWithError()
}


class WatchConnectivityManager:  NSObject, WCSessionDelegate {
    
    fileprivate let session: WCSession
    
    var numberOfOutstandingFileTransfers: Int {
        session.outstandingFileTransfers.count
    }
    
    init(session: WCSession = .default) {
        self.session = session
        super.init()
        if WCSession.isSupported() {
            session.delegate = self
            session.activate()
        }
    }
    
    
    func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        if let error = error {
            print("Error during Watch Connectivity activation: \(error.localizedDescription)")
        } else {
            print("Watch Connectivity session activated without error.")
        }
    }
}


// MARK: - Transfering data

extension WatchConnectivityManager {
    func transferToPhone(url: URL) {
        session.transferFile(url, metadata: nil)
    }
    
    func cancelAllFileTransfers() {
        for f in session.outstandingFileTransfers {
            f.cancel()
        }
    }
    
    func session(_ session: WCSession, didFinish fileTransfer: WCSessionFileTransfer, error: Error?) {
        if let error = error {
            print("error on file transfer: \(error.localizedDescription)")
        } else {
            print("file transfer complete")
        }
    }
}
