//
//  Extensions.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 10/3/20.
//

import Foundation

// MARK: - Encoable extensions
private extension Encodable {
    func encode(to container: inout SingleValueEncodingContainer) throws {
        try container.encode(self)
    }
}

extension JSONEncoder {
    private struct EncodableWrapper: Encodable {
        let wrapped: Encodable

        func encode(to encoder: Encoder) throws {
            var container = encoder.singleValueContainer()
            try self.wrapped.encode(to: &container)
        }
    }

    func encode<Key: Encodable>(_ dictionary: [Key: Encodable]) throws -> Data {
        let wrappedDict = dictionary.mapValues(EncodableWrapper.init(wrapped:))
        return try self.encode(wrappedDict)
    }
}
