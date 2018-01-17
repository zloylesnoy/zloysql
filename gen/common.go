package zloysql

import "github.com/shopspring/decimal"

type DialectSQL int

const (
	MySQL DialectSQL = iota
	MicrosoftSQL
	PostgreSQL
)

type MaybeInt8 struct {
	Value int8
	Null  bool
}

type MaybeInt16 struct {
	Value int16
	Null  bool
}

type MaybeInt32 struct {
	Value int32
	Null  bool
}

type MaybeInt64 struct {
	Value int64
	Null  bool
}

type MaybeUInt8 struct {
	Value uint8
	Null  bool
}

type MaybeUInt16 struct {
	Value uint16
	Null  bool
}

type MaybeUInt32 struct {
	Value uint32
	Null  bool
}

type MaybeUInt64 struct {
	Value uint64
	Null  bool
}

type MaybeFloat32 struct {
	Value float32
	Null  bool
}

type MaybeFloat64 struct {
	Value float64
	Null  bool
}

type MaybeString struct {
	Value string
	Null  bool
}

type MaybeDecimal struct {
	Value decimal.Decimal
	Null  bool
}
