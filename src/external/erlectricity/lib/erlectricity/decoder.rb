module Erlectricity
  class Decoder
    attr_accessor :in
    include Erlectricity::External::Types

    def self.decode(string)
      new(StringIO.new(string)).read_any
    end

    def initialize(ins)
      @in = ins
      @peeked = ""
    end

    def read_any
      fail("Bad Magic") unless read_1 == Erlectricity::External::VERSION
      read_any_raw
    end

    def read_any_raw
      case peek_1
        when ATOM then read_atom
        when SMALL_INT then read_small_int
        when INT then read_int
        when SMALL_BIGNUM then read_small_bignum
        when LARGE_BIGNUM then read_large_bignum
        when FLOAT then read_float
        when NEW_REF then read_new_reference
        when PID then read_pid
        when SMALL_TUPLE then read_small_tuple
        when LARGE_TUPLE then read_large_tuple
        when NIL then read_nil
        when STRING then read_erl_string
        when LIST then read_list
        when BIN then read_bin
        else
          fail("Unknown term tag: #{peek_1}")
      end
    end

    def read(length)
      if length < @peeked.length
        result = @peeked[0...length]
        @peeked = @peeked[length..-1]
        length = 0
      else
        result = @peeked
        @peeked = ''
        length -= result.length
      end

      if length > 0
        result << @in.read(length)
      end
      result
    end

    def peek(length)
      if length <= @peeked.length
        @peeked[0...length]
      else
        read_bytes = @in.read(length - @peeked.length)
        @peeked << read_bytes if read_bytes
        @peeked
      end
    end

    def peek_1
      peek(1).unpack("C").first
    end

    def peek_2
      peek(2).unpack("n").first
    end

    def read_1
      read(1).unpack("C").first
    end

    def read_2
      read(2).unpack("n").first
    end

    def read_4
      read(4).unpack("N").first
    end

    def read_string(length)
      read(length)
    end

    def read_atom
      fail("Invalid Type, not an atom") unless read_1 == ATOM
      length = read_2
      a = read_string(length)
      case a
        when "true"
          true
        when "false"
          false
        when ""
          Marshal.load("\004\b:\005") # Workaround for inability to do ''.to_sym
        else
          a.to_sym
      end
    end

    def read_small_int
      fail("Invalid Type, not a small int") unless read_1 == SMALL_INT
      read_1
    end

    def read_int
      fail("Invalid Type, not an int") unless read_1 == INT
      value = read_4
      negative = (value >> 31)[0] == 1
      value = (value - (1 << 32)) if negative
      value = Fixnum.induced_from(value)
    end

    def read_small_bignum
      fail("Invalid Type, not a small bignum") unless read_1 == SMALL_BIGNUM
      size = read_1
      sign = read_1
      bytes = read_string(size).unpack("C" * size)
      added = bytes.zip((0..bytes.length).to_a).inject(0) do |result, byte_index|
        byte, index = *byte_index
        value = (byte * (256 ** index))
        sign != 0 ? (result - value) : (result + value)
      end
      Bignum.induced_from(added)
    end

    def read_large_bignum
      fail("Invalid Type, not a large bignum") unless read_1 == LARGE_BIGNUM
      size = read_4
      sign = read_1
      bytes = read_string(size).unpack("C" * size)
      added = bytes.zip((0..bytes.length).to_a).inject(0) do |result, byte_index|
        byte, index = *byte_index
        value = (byte * (256 ** index))
        sign != 0 ? (result - value) : (result + value)
      end
      Bignum.induced_from(added)
    end

    def read_float
      fail("Invalid Type, not a float") unless read_1 == FLOAT
      string_value = read_string(31)
      result = string_value.to_f
    end

    def read_new_reference
      fail("Invalid Type, not a new-style reference") unless read_1 == NEW_REF
      size = read_2
      node = read_atom
      creation = read_1
      id = (0...size).map { |i| read_4 }
      NewReference.new(node, creation, id)
    end

    def read_pid
      fail("Invalid Type, not a pid") unless read_1 == PID
      node = read_atom
      id = read_4
      serial = read_4
      creation = read_1
      Pid.new(node, id, serial, creation)
    end

    def read_small_tuple
      fail("Invalid Type, not a small tuple") unless read_1 == SMALL_TUPLE
      arity = read_1
      (0...arity).map { |i| read_any_raw }
    end

    def read_large_tuple
      fail("Invalid Type, not a small tuple") unless read_1 == LARGE_TUPLE
      arity = read_4
      (0...arity).map { |i| read_any_raw }
    end

    def read_nil
      fail("Invalid Type, not a nil list") unless read_1 == NIL
      Erlectricity::List.new([])
    end

    def read_erl_string
      fail("Invalid Type, not an erlang string") unless read_1 == STRING
      length = read_2
      Erlectricity::List.new(read_string(length).unpack('C' * length))
    end

    def read_list
      fail("Invalid Type, not an erlang list") unless read_1 == LIST
      length = read_4
      list = (0...length).map { |i| read_any_raw }
      read_1
      Erlectricity::List.new(list)
    end

    def read_bin
      fail("Invalid Type, not an erlang binary") unless read_1 == BIN
      length = read_4
      read_string(length)
    end

    def fail(str)
      raise DecodeError, str
    end
  end
end
