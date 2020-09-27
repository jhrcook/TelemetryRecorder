//
//  TaskCheckView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/27/20.
//

import SwiftUI

enum TaskStatus {
    case incomplete, inProgress, complete
}

struct TaskCheckView: View {
    var text: String
    @Binding var status: TaskStatus
    
    private var imageName: String {
        switch status {
        case .incomplete:
            return "multiply.circle"
        case .inProgress:
            return "arrow.2.circlepath.circle"
        case .complete:
            return "checkmark.circle"
        }
    }
    
    private var imageColor: Color {
        switch status {
        case .incomplete:
            return Color.red
        case .inProgress:
            return Color.yellow
        case .complete:
            return Color.green
        }
    }
    
    var body: some View {
        HStack {
            Text(text)
            Spacer()
            Image(systemName: imageName)
                .foregroundColor(imageColor)
        }
    }
}

struct TaskCheckView_Previews: PreviewProvider {
    static var previews: some View {
        VStack {
            TaskCheckView(text: "Incomplete", status: .constant(.inProgress))
            TaskCheckView(text: "In progress", status: .constant(.inProgress))
            TaskCheckView(text: "Complete", status: .constant(.complete))
        }
        .previewLayout(.sizeThatFits)
    }
}
