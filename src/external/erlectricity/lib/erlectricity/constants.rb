module Erlectricity
  module External
    module  Types
      SMALL_INT = 97
      INT = 98

      SMALL_BIGNUM = 110
      LARGE_BIGNUM = 111

      FLOAT = 99

      ATOM = 100
      REF = 101           #old style reference
      NEW_REF = 114
      PORT = 102          #not supported accross node boundaries
      PID = 103

      SMALL_TUPLE = 104
      LARGE_TUPLE = 105

      NIL = 106
      STRING = 107
      LIST = 108
      BIN = 109

      FUN = 117
      NEW_FUN = 112
    end

    VERSION = 131

    MAX_INT = (1 << 27) -1
    MIN_INT = -(1 << 27)
    MAX_ATOM = 255
  end
end