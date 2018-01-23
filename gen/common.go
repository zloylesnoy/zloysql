package zloysql

import (
	"database/sql"
	"errors"
	"math/big"

	"github.com/shopspring/decimal"
)

var ErrorSqlDbIsNil error = errors.New("sql.DB is nil pointer")

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

func (it MaybeInt8) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeInt16 struct {
	Value int16
	Null  bool
}

func (it MaybeInt16) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeInt32 struct {
	Value int32
	Null  bool
}

func (it MaybeInt32) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeInt64 struct {
	Value int64
	Null  bool
}

func (it MaybeInt64) ToSql() sql.NullInt64 {
	return sql.NullInt64{it.Value, !it.Null}
}

type MaybeUInt8 struct {
	Value uint8
	Null  bool
}

func (it MaybeUInt8) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeUInt16 struct {
	Value uint16
	Null  bool
}

func (it MaybeUInt16) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeUInt32 struct {
	Value uint32
	Null  bool
}

func (it MaybeUInt32) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeUInt64 struct {
	Value uint64
	Null  bool
}

func (it MaybeUInt64) ToSql() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

type MaybeFloat32 struct {
	Value float32
	Null  bool
}

func (it MaybeFloat32) ToSql() sql.NullFloat64 {
	return sql.NullFloat64{float64(it.Value), !it.Null}
}

type MaybeFloat64 struct {
	Value float64
	Null  bool
}

func (it MaybeFloat64) ToSql() sql.NullFloat64 {
	return sql.NullFloat64{it.Value, !it.Null}
}

type MaybeString struct {
	Value string
	Null  bool
}

func (it MaybeString) ToSql() sql.NullString {
	return sql.NullString{it.Value, !it.Null}
}

type MaybeDecimal struct {
	Value decimal.Decimal
	Null  bool
}

func (it MaybeDecimal) ToSql() []byte {
	if it.Null {
		return nil
	}
	return []byte(it.Value.String())
}

func BigIntToBytes(a *big.Int) []byte {
	if a == nil {
		return nil
	}
	return []byte(a.String())
}

func DecimalToBytes(a decimal.Decimal) []byte {
	return []byte(a.String())
}
