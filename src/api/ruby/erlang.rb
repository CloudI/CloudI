#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2019 Michael Truog <mjtruog at protonmail dot com>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

require 'zlib'

# Erlang term classes listed alphabetically

module Erlang

    class OtpErlangAtom
        def initialize(value)
            @value = value
        end
        attr_reader :value
        def binary
            if @value.kind_of?(Integer)
                return "#{TAG_ATOM_CACHE_REF.chr}#{@value.chr}"
            elsif @value.kind_of?(String)
                length = @value.bytesize
                if @value.encoding.name == 'UTF-8'
                    if length <= 255
                        return "#{TAG_SMALL_ATOM_UTF8_EXT.chr}#{length.chr}" \
                               "#{@value}"
                    elsif length <= 65535
                        length_packed = [length].pack('n')
                        return "#{TAG_ATOM_UTF8_EXT.chr}#{length_packed}" \
                               "#{@value}"
                    else
                        raise OutputException, 'uint16 overflow', caller
                    end
                else
                    if length <= 255
                        return "#{TAG_SMALL_ATOM_EXT.chr}#{length.chr}#{@value}"
                    elsif length <= 65535
                        length_packed = [length].pack('n')
                        return "#{TAG_ATOM_EXT.chr}#{length_packed}#{@value}"
                    else
                        raise OutputException, 'uint16 overflow', caller
                    end
                end
            else
                raise OutputException, 'unknown atom type', caller
            end
        end
        def to_s
            return "#{self.class.name}('#{@value.to_s}')"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end
    
    class OtpErlangBinary
        def initialize(value, bits = 8)
            @value = value
            @bits = bits # bits in last byte
        end
        attr_reader :value
        attr_reader :bits
        def binary
            if @value.kind_of?(String)
                length = @value.bytesize
                if length > 4294967295
                    raise OutputException, 'uint32 overflow', caller
                elsif @bits != 8
                    length_packed = [length].pack('N')
                    return "#{TAG_BIT_BINARY_EXT.chr}#{length_packed}" \
                           "#{@bits.chr}#{@value}"
                else
                    length_packed = [length].pack('N')
                    return "#{TAG_BINARY_EXT.chr}#{length_packed}#{@value}"
                end
            else
                raise OutputException, 'unknown binary type', caller
            end
        end
        def to_s
            return "#{self.class.name}('#{@value.to_s}',bits=#{@bits.to_s})"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end
    
    class OtpErlangFunction
        def initialize(tag, value)
            @tag = tag
            @value = value
        end
        attr_reader :tag
        attr_reader :value
        def binary
            return "#{@tag.chr}#{@value}"
        end
        def to_s
            return "#{self.class.name}('#{@tag.to_s}','#{@value.to_s}')"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end
    
    class OtpErlangList
        def initialize(value, improper = false)
            @value = value
            @improper = improper
        end
        attr_reader :value
        attr_reader :improper
        def binary
            if @value.kind_of?(Array)
                length = @value.length
                if length == 0
                    return TAG_NIL_EXT.chr
                elsif length > 4294967295
                    raise OutputException, 'uint32 overflow', caller
                elsif @improper
                    length_packed = [length - 1].pack('N')
                    list_packed = @value.map{ |element|
                        Erlang::term_to_binary_(element)
                    }.join
                    return "#{TAG_LIST_EXT.chr}#{length_packed}#{list_packed}"
                else
                    length_packed = [length].pack('N')
                    list_packed = @value.map{ |element|
                        Erlang::term_to_binary_(element)
                    }.join
                    return "#{TAG_LIST_EXT.chr}#{length_packed}" \
                           "#{list_packed}#{TAG_NIL_EXT.chr}"
                end
            else
                raise OutputException, 'unknown list type', caller
            end
        end
        def to_s
            return "#{self.class.name}" \
                   "(#{@value.to_s},improper=#{@improper.to_s})"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end

    class OtpErlangPid
        def initialize(node, id, serial, creation)
            @node = node
            @id = id
            @serial = serial
            @creation = creation
        end
        attr_reader :node
        attr_reader :id
        attr_reader :serial
        attr_reader :creation
        def binary
            creation_size = @creation.bytesize
            if creation_size == 1
                return "#{TAG_PID_EXT.chr}" \
                       "#{@node.binary}#{@id}#{@serial}#{@creation}"
            elsif creation_size == 4
                return "#{TAG_NEW_PID_EXT.chr}" \
                       "#{@node.binary}#{@id}#{@serial}#{@creation}"
            else
                raise OutputException, 'unknown pid type', caller
            end
        end
        def to_s
            return "#{self.class.name}" \
                   "('#{@node.to_s}','#{@id.to_s}','#{@serial.to_s}'," \
                    "'#{@creation.to_s}')"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end
    
    class OtpErlangPort
        def initialize(node, id, creation)
            @node = node
            @id = id
            @creation = creation
        end
        attr_reader :node
        attr_reader :id
        attr_reader :creation
        def binary
            creation_size = @creation.bytesize
            if creation_size == 1
                return "#{TAG_PORT_EXT.chr}" \
                       "#{@node.binary}#{@id}#{@creation}"
            elsif creation_size == 4
                return "#{TAG_NEW_PORT_EXT.chr}" \
                       "#{@node.binary}#{@id}#{@creation}"
            else
                raise OutputException, 'unknown port type', caller
            end
        end
        def to_s
            return "#{self.class.name}" \
                   "('#{@node.to_s}','#{@id.to_s}','#{@creation.to_s}')"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end
    
    class OtpErlangReference
        def initialize(node, id, creation)
            @node = node
            @id = id
            @creation = creation
        end
        attr_reader :node
        attr_reader :id
        attr_reader :creation
        def binary
            length = @id.bytesize / 4
            if length == 0
                return "#{TAG_REFERENCE_EXT.chr}" \
                       "#{@node.binary}#{@id}#{@creation}"
            elsif length <= 65535
                length_packed = [length].pack('n')
                creation_size = @creation.bytesize
                if creation_size == 1
                    return "#{TAG_NEW_REFERENCE_EXT.chr}#{length_packed}" \
                           "#{@node.binary}#{@creation}#{@id}"
                elsif creation_size == 4
                    return "#{TAG_NEWER_REFERENCE_EXT.chr}#{length_packed}" \
                           "#{@node.binary}#{@creation}#{@id}"
                else
                    raise OutputException, 'unknown reference type', caller
                end
            else
                raise OutputException, 'uint16 overflow', caller
            end
        end
        def to_s
            return "#{self.class.name}" \
                   "('#{@node.to_s}','#{@id.to_s}','#{@creation.to_s}')"
        end
        def hash
            return binary.hash
        end
        def ==(other)
            return binary == other.binary
        end
        alias eql? ==
    end

    # core functionality
    
    def self.binary_to_term(data)
        size = data.bytesize
        if size <= 1
            raise ParseException, 'null input', caller
        end
        if data[0].ord != TAG_VERSION
            raise ParseException, 'invalid version', caller
        end
        begin
            result = binary_to_term_(1, data)
            if result[0] != size
                raise ParseException, 'unparsed data', caller
            end
            return result[1]
        rescue NoMethodError => e
            raise ParseException, 'missing data', e.backtrace
        rescue TypeError => e
            raise ParseException, 'missing data', e.backtrace
        rescue ArgumentError => e
            raise ParseException, 'missing data', e.backtrace
        end
    end
    
    def self.term_to_binary(term, compressed = false)
        data_uncompressed = term_to_binary_(term)
        if compressed == false
            return "#{TAG_VERSION.chr}#{data_uncompressed}"
        else
            if compressed == true
                compressed = 6
            end
            if compressed < 0 or compressed > 9
                raise InputException, 'compressed in [0..9]', caller
            end
            data_compressed = Zlib::Deflate.deflate(data_uncompressed,
                                                    compressed)
            size_uncompressed = data_uncompressed.bytesize
            if size_uncompressed > 4294967295
                raise OutputException, 'uint32 overflow', caller
            end
            size_uncompressed_packed = [size_uncompressed].pack('N')
            return "#{TAG_VERSION.chr}#{TAG_COMPRESSED_ZLIB.chr}" \
                   "#{size_uncompressed_packed}#{data_compressed}"
        end
    end
    
    private
    
    # binary_to_term implementation functions
    
    def self.binary_to_term_(i, data)
        tag = data[i].ord
        i += 1
        if tag == TAG_NEW_FLOAT_EXT
            return [i + 8, data[i,8].unpack('G')[0]]
        elsif tag == TAG_BIT_BINARY_EXT
            j = data[i,4].unpack('N')[0]
            i += 4
            bits = data[i].ord
            i += 1
            return [i + j, OtpErlangBinary.new(data[i,j], bits)]
        elsif tag == TAG_ATOM_CACHE_REF
            return [i + 1, OtpErlangAtom.new(ord(data[i,1]))]
        elsif tag == TAG_SMALL_INTEGER_EXT
            return [i + 1, data[i].ord]
        elsif tag == TAG_INTEGER_EXT
            value = data[i,4].unpack('N')[0]
            if 0 != (value & 0x80000000)
                value = -2147483648 + (value & 0x7fffffff)
            end
            return [i + 4, value]
        elsif tag == TAG_FLOAT_EXT
            value = data[i,31].partition(0.chr)[0].to_f
            return [i + 31, value]
        elsif tag == TAG_ATOM_EXT
            j = data[i,2].unpack('n')[0]
            i += 2
            atom_name = data[i,j].force_encoding('ISO-8859-1')
            if atom_name.valid_encoding?
                return [i + j, atom_name.to_sym]
            else
                raise ParseException, 'invalid atom_name latin1', caller
            end
        elsif tag == TAG_NEW_PORT_EXT or \
              tag == TAG_REFERENCE_EXT or tag == TAG_PORT_EXT
            result = binary_to_atom(i, data)
            i = result[0]; node = result[1]
            id = data[i,4]
            i += 4
            if tag == TAG_NEW_PORT_EXT
                creation = data[i,4]
                i += 4
            else
                creation = data[i]
                i += 1
                if tag == TAG_REFERENCE_EXT
                    return [i, OtpErlangReference.new(node, id, creation)]
                end
            end
            # tag == TAG_NEW_PORT_EXT or tag == TAG_PORT_EXT
            return [i, OtpErlangPort.new(node, id, creation)]
        elsif tag == TAG_NEW_PID_EXT or tag == TAG_PID_EXT
            result = binary_to_atom(i, data)
            i = result[0]; node = result[1]
            id = data[i,4]
            i += 4
            serial = data[i,4]
            i += 4
            if tag == TAG_NEW_PID_EXT
                creation = data[i,4]
                i += 4
            elsif tag == TAG_PID_EXT
                creation = data[i]
                i += 1
            end
            return [i, OtpErlangPid.new(node, id, serial, creation)]
        elsif tag == TAG_SMALL_TUPLE_EXT or tag == TAG_LARGE_TUPLE_EXT
            if tag == TAG_SMALL_TUPLE_EXT
                length = data[i].ord
                i += 1
            elsif tag == TAG_LARGE_TUPLE_EXT
                length = data[i,4].unpack('N')[0]
                i += 4
            end
            result = binary_to_term_sequence(i, length, data)
            i = result[0]; tmp = result[1]
            return [i, tmp]
        elsif tag == TAG_NIL_EXT
            return [i, OtpErlangList.new([])]
        elsif tag == TAG_STRING_EXT
            j = data[i,2].unpack('n')[0]
            i += 2
            return [i + j, data[i,j]]
        elsif tag == TAG_LIST_EXT
            length = data[i,4].unpack('N')[0]
            i += 4
            result = binary_to_term_sequence(i, length, data)
            i = result[0]; tmp = result[1]
            result = binary_to_term_(i, data)
            i = result[0]; tail = result[1]
            if tail.kind_of?(OtpErlangList) == false or tail.value != []
                tmp.push(tail)
                tmp = OtpErlangList.new(tmp, true)
            else
                tmp = OtpErlangList.new(tmp)
            end
            return [i, tmp]
        elsif tag == TAG_BINARY_EXT
            j = data[i,4].unpack('N')[0]
            i += 4
            return [i + j, OtpErlangBinary.new(data[i,j], 8)]
        elsif tag == TAG_SMALL_BIG_EXT or tag == TAG_LARGE_BIG_EXT
            if tag == TAG_SMALL_BIG_EXT
                j = data[i].ord
                i += 1
            elsif tag == TAG_LARGE_BIG_EXT
                j = data[i,4].unpack('N')[0]
                i += 4
            end
            sign = data[i].ord
            bignum = 0
            (0...j).each do |bignum_index|
                digit = data[i + j - bignum_index].ord
                bignum = bignum * 256 + digit
            end
            if sign == 1
                bignum *= -1
            end
            i += 1
            return [i + j, bignum]
        elsif tag == TAG_NEW_FUN_EXT
            length = data[i,4].unpack('N')[0]
            return [i + length, OtpErlangFunction.new(tag, data[i,length])]
        elsif tag == TAG_EXPORT_EXT
            old_i = i
            result = binary_to_atom(i, data)
            i = result[0]#; name_module = result[1]
            result = binary_to_atom(i, data)
            i = result[0]#; function = result[1]
            if data[i].ord != TAG_SMALL_INTEGER_EXT
                raise ParseException, 'invalid small integer tag', caller
            end
            i += 1
            #arity = data[i].ord
            i += 1
            return [i, OtpErlangFunction.new(tag, data[old_i,i - old_i])]
        elsif tag == TAG_NEWER_REFERENCE_EXT or tag == TAG_NEW_REFERENCE_EXT
            j = data[i,2].unpack('n')[0] * 4
            i += 2
            result = binary_to_atom(i, data)
            i = result[0]; node = result[1]
            if tag == TAG_NEWER_REFERENCE_EXT
                creation = data[i,4]
                i += 4
            elsif tag == TAG_NEW_REFERENCE_EXT
                creation = data[i]
                i += 1
            end
            id = data[i,j]
            return [i + j, OtpErlangReference.new(node, id, creation)]
        elsif tag == TAG_SMALL_ATOM_EXT
            j = data[i,1].ord
            i += 1
            atom_name = data[i,j].force_encoding('ISO-8859-1')
            if atom_name.valid_encoding?
                if atom_name == 'true'
                    tmp = true
                elsif atom_name == 'false'
                    tmp = false
                else
                    tmp = atom_name.to_sym
                end
                return [i + j, tmp]
            else
                raise ParseException, 'invalid atom_name latin1', caller
            end
        elsif tag == TAG_MAP_EXT
            length = data[i,4].unpack('N')[0]
            i += 4
            pairs = Hash.new
            (0...length).each do |length_index|
                result = binary_to_term_(i, data)
                i = result[0]; key = result[1]
                result = binary_to_term_(i, data)
                i = result[0]; value = result[1]
                pairs[key] = value
            end
            return [i, pairs]
        elsif tag == TAG_FUN_EXT
            old_i = i
            #numfree = data[i,4].unpack('N')[0]
            i += 4
            result = binary_to_pid(i, data)
            i = result[0]#; pid = result[1]
            result = binary_to_atom(i, data)
            i = result[0]#; name_module = result[1]
            result = binary_to_integer(i, data)
            i = result[0]#; index = result[1]
            result = binary_to_integer(i, data)
            i = result[0]#; uniq = result[1]
            result = binary_to_term_sequence(i, numfree, data)
            i = result[0]#; free = result[1]
            return [i, OtpErlangFunction.new(tag, data[old_i,i - old_i])]
        elsif tag == TAG_ATOM_UTF8_EXT
            j = data[i,2].unpack('n')[0]
            i += 2
            atom_name = data[i,j].force_encoding('UTF-8')
            if atom_name.valid_encoding?
                return [i + j, OtpErlangAtom.new(atom_name)]
            else
                raise ParseException, 'invalid atom_name unicode', caller
            end
        elsif tag == TAG_SMALL_ATOM_UTF8_EXT
            j = data[i,1].ord
            i += 1
            atom_name = data[i,j].force_encoding('UTF-8')
            if atom_name.valid_encoding?
                return [i + j, OtpErlangAtom.new(atom_name)]
            else
                raise ParseException, 'invalid atom_name unicode', caller
            end
        elsif tag == TAG_COMPRESSED_ZLIB
            size_uncompressed = data[i,4].unpack('N')[0]
            if size_uncompressed == 0
                raise ParseException, 'compressed data null', caller
            end
            i += 4
            data_compressed = data[i..-1]
            j = data_compressed.bytesize
            data_uncompressed = Zlib::Inflate.inflate(data_compressed)
            if size_uncompressed != data_uncompressed.bytesize
                raise ParseException, 'compression corrupt', caller
            end
            result = binary_to_term_(0, data_uncompressed)
            i_new = result[0]; term = result[1]
            if i_new != size_uncompressed
                raise ParseException, 'unparsed data', caller
            end
            return [i + j, term]
        else
            raise ParseException, 'invalid tag', caller
        end
    end
    
    def self.binary_to_term_sequence(i, length, data)
        sequence = []
        (0...length).each do |length_index|
            result = binary_to_term_(i, data)
            i = result[0]; element = result[1]
            sequence.push(element)
        end
        return [i, sequence]
    end

    # (binary_to_term Erlang term primitive type functions)
            
    def self.binary_to_integer(i, data)
        tag = data[i].ord
        i += 1
        if tag == TAG_SMALL_INTEGER_EXT
            return [i + 1, data[i].ord]
        elsif tag == TAG_INTEGER_EXT
            value = data[i,4].unpack('N')[0]
            if 0 != (value & 0x80000000)
                value = -2147483648 + (value & 0x7fffffff)
            end
            return [i + 4, Fixnum.induced_from(value)]
        else
            raise ParseException, 'invalid integer tag', caller
        end
    end
    
    def self.binary_to_pid(i, data)
        tag = data[i].ord
        i += 1
        if tag == TAG_NEW_PID_EXT
            result = binary_to_atom(i, data)
            i = result[0]; node = result[1]
            id = data[i,4]
            i += 4
            serial = data[i,4]
            i += 4
            creation = data[i,4]
            i += 4
            return [i, OtpErlangPid.new(node, id, serial, creation)]
        elsif tag == TAG_PID_EXT
            result = binary_to_atom(i, data)
            i = result[0]; node = result[1]
            id = data[i,4]
            i += 4
            serial = data[i,4]
            i += 4
            creation = data[i]
            i += 1
            return [i, OtpErlangPid.new(node, id, serial, creation)]
        else
            raise ParseException, 'invalid pid tag', caller
        end
    end
    
    def self.binary_to_atom(i, data)
        tag = data[i].ord
        i += 1
        if tag == TAG_ATOM_EXT
            j = data[i,2].unpack('n')[0]
            i += 2
            return [i + j, OtpErlangAtom.new(data[i,j])]
        elsif tag == TAG_ATOM_CACHE_REF
            return [i + 1, OtpErlangAtom.new(data[i,1].ord)]
        elsif tag == TAG_SMALL_ATOM_EXT
            j = data[i,1].ord
            i += 1
            return [i + j, OtpErlangAtom.new(data[i,j])]
        elsif tag == TAG_ATOM_UTF8_EXT
            j = data[i,2].unpack('n')[0]
            i += 2
            atom_name = data[i,j].force_encoding('UTF-8')
            if atom_name.valid_encoding?
                return [i + j, OtpErlangAtom.new(atom_name)]
            else
                raise ParseException, 'invalid atom_name unicode', caller
            end
        elsif tag == TAG_SMALL_ATOM_UTF8_EXT
            j = data[i,1].ord
            i += 1
            atom_name = data[i,j].force_encoding('UTF-8')
            if atom_name.valid_encoding?
                return [i + j, OtpErlangAtom.new(atom_name)]
            else
                raise ParseException, 'invalid atom_name unicode', caller
            end
        else
            raise ParseException, 'invalid atom tag', caller
        end
    end
    
    # term_to_binary implementation functions
    
    def self.term_to_binary_(term)
        if term.kind_of?(String)
            return string_to_binary(term)
        elsif term.kind_of?(Array)
            return tuple_to_binary(term)
        elsif term.kind_of?(Float)
            return float_to_binary(term)
        elsif term.kind_of?(Integer)
            return integer_to_binary(term)
        elsif term.kind_of?(Hash)
            return hash_to_binary(term)
        elsif term.kind_of?(Symbol)
            return OtpErlangAtom.new(term.to_s).binary
        elsif term.kind_of?(TrueClass)
            return OtpErlangAtom.new(
                'true'.force_encoding('ISO-8859-1')
            ).binary
        elsif term.kind_of?(FalseClass)
            return OtpErlangAtom.new(
                'false'.force_encoding('ISO-8859-1')
            ).binary
        elsif term.nil?
            return OtpErlangAtom.new(
                'undefined'.force_encoding('ISO-8859-1')
            ).binary
        elsif term.kind_of?(OtpErlangAtom)
            return term.binary
        elsif term.kind_of?(OtpErlangList)
            return term.binary
        elsif term.kind_of?(OtpErlangBinary)
            return term.binary
        elsif term.kind_of?(OtpErlangFunction)
            return term.binary
        elsif term.kind_of?(OtpErlangReference)
            return term.binary
        elsif term.kind_of?(OtpErlangPort)
            return term.binary
        elsif term.kind_of?(OtpErlangPid)
            return term.binary
        else
            raise OutputException, 'unknown ruby type', caller
        end
    end

    # (term_to_binary Erlang term composite type functions)
    
    def self.string_to_binary(term)
        length = term.bytesize
        if length == 0
            return TAG_NIL_EXT.chr
        elsif length <= 65535
            length_packed = [length].pack('n')
            return "#{TAG_STRING_EXT.chr}#{length_packed}#{term}"
        elsif length <= 4294967295
            length_packed = [length].pack('N')
            term_packed = term.unpack("C#{length}").map{ |c|
                "#{TAG_SMALL_INTEGER_EXT.chr}#{c}"
            }.join
            return "#{TAG_LIST_EXT.chr}#{length_packed}#{term_packed}" \
                   "#{TAG_NIL_EXT.chr}"
        else
            raise OutputException, 'uint32 overflow', caller
        end
    end
    
    def self.tuple_to_binary(term)
        length = term.length
        term_packed = term.map{ |element|
            term_to_binary_(element)
        }.join
        if length <= 255
            return "#{TAG_SMALL_TUPLE_EXT.chr}#{length.chr}#{term_packed}"
        elsif length <= 4294967295
            length_packed = [length].pack('N')
            return "#{TAG_LARGE_TUPLE_EXT.chr}#{length_packed}#{term_packed}"
        else
            raise OutputException, 'uint32 overflow', caller
        end
    end
    
    def self.hash_to_binary(term)
        length = term.length
        if length <= 4294967295
            term_packed = term.to_a.map{ |element|
                key_packed = term_to_binary_(element[0])
                value_packed = term_to_binary_(element[1])
                "#{key_packed}#{value_packed}"
            }.join
            length_packed = [length].pack('N')
            return "#{TAG_MAP_EXT.chr}#{length_packed}#{term_packed}"
        else
            raise OutputException, 'uint32 overflow', caller
        end
    end

    # (term_to_binary Erlang term primitive type functions)

    def self.integer_to_binary(term)
        if 0 <= term and term <= 255
            return "#{TAG_SMALL_INTEGER_EXT.chr}#{term.chr}"
        elsif -2147483648 <= term and term <= 2147483647
            term_packed = [term].pack('N')
            return "#{TAG_INTEGER_EXT.chr}#{term_packed}"
        else
            return bignum_to_binary(term)
        end
    end
    
    def self.bignum_to_binary(term)
        bignum = term.abs
        if term < 0
            sign = 1.chr
        else
            sign = 0.chr
        end
        l = []
        while bignum > 0 do
            l.push((bignum & 255).chr)
            bignum >>= 8
        end
        length = l.length
        if length <= 255
            return "#{TAG_SMALL_BIG_EXT.chr}#{length.chr}#{sign}#{l.join}"
        elsif length <= 4294967295
            length_packed = [length].pack('N')
            return "#{TAG_LARGE_BIG_EXT.chr}#{length_packed}#{sign}#{l.join}"
        else
            raise OutputException, 'uint32 overflow', caller
        end
    end

    def self.float_to_binary(term)
        term_packed = [term].pack('G')
        return "#{TAG_NEW_FLOAT_EXT.chr}#{term_packed}"
    end

    # Exception classes listed alphabetically
    
    class InputException < ArgumentError
    end

    class OutputException < TypeError
    end

    class ParseException < SyntaxError
    end
    
    # tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
    TAG_VERSION = 131
    TAG_COMPRESSED_ZLIB = 80
    TAG_NEW_FLOAT_EXT = 70
    TAG_BIT_BINARY_EXT = 77
    TAG_ATOM_CACHE_REF = 78
    TAG_NEW_PID_EXT = 88
    TAG_NEW_PORT_EXT = 89
    TAG_NEWER_REFERENCE_EXT = 90
    TAG_SMALL_INTEGER_EXT = 97
    TAG_INTEGER_EXT = 98
    TAG_FLOAT_EXT = 99
    TAG_ATOM_EXT = 100
    TAG_REFERENCE_EXT = 101
    TAG_PORT_EXT = 102
    TAG_PID_EXT = 103
    TAG_SMALL_TUPLE_EXT = 104
    TAG_LARGE_TUPLE_EXT = 105
    TAG_NIL_EXT = 106
    TAG_STRING_EXT = 107
    TAG_LIST_EXT = 108
    TAG_BINARY_EXT = 109
    TAG_SMALL_BIG_EXT = 110
    TAG_LARGE_BIG_EXT = 111
    TAG_NEW_FUN_EXT = 112
    TAG_EXPORT_EXT = 113
    TAG_NEW_REFERENCE_EXT = 114
    TAG_SMALL_ATOM_EXT = 115
    TAG_MAP_EXT = 116
    TAG_FUN_EXT = 117
    TAG_ATOM_UTF8_EXT = 118
    TAG_SMALL_ATOM_UTF8_EXT = 119

end

