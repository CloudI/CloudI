module Erlectricity
  class Encoder
    include Erlectricity::External::Types

    attr_accessor :out

    def initialize(out)
      self.out = out
    end

    def self.encode(data)
      io = StringIO.new
      self.new(io).write_any(data)
      io.string
    end

    def write_any obj
      write_1 Erlectricity::External::VERSION
      write_any_raw obj
    end

    def write_any_raw obj
      case obj
        when Symbol then write_symbol(obj)
        when Fixnum, Bignum then write_fixnum(obj)
        when Float then write_float(obj)
        when Erlectricity::NewReference then write_new_reference(obj)
        when Erlectricity::Pid then write_pid(obj)
        when Erlectricity::List then write_list(obj)
        when Array then write_tuple(obj)
        when String then write_binary(obj)
        when Time then write_any_raw(obj.to_i.divmod(1000000) + [obj.usec])
        when TrueClass, FalseClass then write_boolean(obj)
        else
          fail(obj)
      end
    end

    def write_1(byte)
      out.write([byte].pack("C"))
    end

    def write_2(short)
      out.write([short].pack("n"))
    end

    def write_4(long)
      out.write([long].pack("N"))
    end

    def write_string(string)
      out.write(string)
    end

    def write_boolean(bool)
      write_symbol(bool.to_s.to_sym)
    end

    def write_symbol(sym)
      fail(sym) unless sym.is_a?(Symbol)
      data = sym.to_s
      write_1 ATOM
      write_2 data.length
      write_string data
    end

    def write_fixnum(num)
      if num >= 0 && num < 256
        write_1 SMALL_INT
        write_1 num
      elsif num <= Erlectricity::External::MAX_INT && num >= Erlectricity::External::MIN_INT
        write_1 INT
        write_4 num
      else
        write_bignum num
      end
    end

    def write_float(float)
      write_1 FLOAT
      write_string format("%15.15e", float).ljust(31, "\000")
    end

    def write_bignum(num)
      if num.is_a?(Bignum)
        n = num.size
      else
        n = (num.to_s(2).size / 8.0).ceil
      end
      if n <= 256
        write_1 SMALL_BIGNUM
        write_1 n
        write_bignum_guts(num)
      else
        write_1 LARGE_BIGNUM
        write_4 n
        write_bignum_guts(num)
      end
    end

    def write_bignum_guts(num)
      write_1 (num >= 0 ? 0 : 1)
      num = num.abs
      while num != 0
        rem = num % 256
        write_1 rem
        num = num >> 8
      end
    end

    def write_new_reference(ref)
      fail(ref) unless ref.is_a?(Erlectricity::NewReference)
      write_1 NEW_REF
      write_2 ref.id.length
      write_symbol(ref.node)
      write_1 ref.creation
      write_string ref.id.pack('N' * ref.id.length)
    end

    def write_pid(pid)
      fail(pid) unless pid.is_a? Erlectricity::Pid
      write_1 PID
      write_symbol(pid.node)
      write_4 pid.id
      write_4 pid.serial
      write_1 pid.creation
    end

    def write_tuple(data)
      fail(data) unless data.is_a? Array

      if data.length < 256
        write_1 SMALL_TUPLE
        write_1 data.length
      else
        write_1 LARGE_TUPLE
        write_4 data.length
      end

      data.each { |e| write_any_raw e }
    end

    def write_list(data)
      fail(data) unless data.is_a? Array
      write_1 NIL and return if data.empty?
      write_1 LIST
      write_4 data.length
      data.each{|e| write_any_raw e }
      write_1 NIL
    end

    def write_binary(data)
      write_1 BIN
      write_4 data.length
      write_string data
    end

    private

    def fail(obj)
      raise EncodeError, "Cannot encode to erlang external format: #{obj.inspect}"
    end
  end
end
