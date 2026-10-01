"""Independent standard-library IEEE-754 vectors for the versioned identity wire format."""
import json
import math
import struct

def number(value):
    bits = 0x7FF8000000000000 if math.isnan(value) else struct.unpack('>q', struct.pack('>d', value))[0]
    return 'bits:' + str(bits)

def record(tag, *fields):
    return tag + '(' + ','.join(str(len(x.encode('utf-16-le')) // 2) + ':' + x for x in fields) + ')'

def sequence(values):
    return record('sequence', *values)

if __name__ == '__main__':
    values = [0.0, -0.0, 1.0, 5.0, 15.0, 1 / 120, 1e-10, 5e-324, 1.7976931348623157e308]
    spmg = record('spmg', number(5), number(15), number(1 / 120))
    descriptor = ('hrf-descriptor/v2|family=' + record('known', 'spmg1') + '|basis=1|span=' + number(24) +
                  '|params=' + spmg + '|derivative=' + record('spmg', spmg, '1') +
                  '|penalty=identity|integration=' + record('spmg1', spmg) + '|components=sequence()')
    print(json.dumps({'spmg1_descriptor': descriptor,
                      'finite_vectors': [{'value': repr(x), 'encoding': number(x)} for x in values]}, indent=2))
