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

func (it MaybeInt8) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeInt8) FromSqlDriver(a sql.NullInt64) {
	it.Value = int8(a.Int64)
	it.Null = !a.Valid
}

type MaybeInt16 struct {
	Value int16
	Null  bool
}

func (it MaybeInt16) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeInt16) FromSqlDriver(a sql.NullInt64) {
	it.Value = int16(a.Int64)
	it.Null = !a.Valid
}

type MaybeInt32 struct {
	Value int32
	Null  bool
}

func (it MaybeInt32) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeInt32) FromSqlDriver(a sql.NullInt64) {
	it.Value = int32(a.Int64)
	it.Null = !a.Valid
}

type MaybeInt64 struct {
	Value int64
	Null  bool
}

func (it MaybeInt64) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{it.Value, !it.Null}
}

func (it *MaybeInt64) FromSqlDriver(a sql.NullInt64) {
	it.Value = a.Int64
	it.Null = !a.Valid
}

type MaybeUInt8 struct {
	Value uint8
	Null  bool
}

func (it MaybeUInt8) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeUInt8) FromSqlDriver(a sql.NullInt64) {
	it.Value = uint8(a.Int64)
	it.Null = !a.Valid
}

type MaybeUInt16 struct {
	Value uint16
	Null  bool
}

func (it MaybeUInt16) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeUInt16) FromSqlDriver(a sql.NullInt64) {
	it.Value = uint16(a.Int64)
	it.Null = !a.Valid
}

type MaybeUInt32 struct {
	Value uint32
	Null  bool
}

func (it MaybeUInt32) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeUInt32) FromSqlDriver(a sql.NullInt64) {
	it.Value = uint32(a.Int64)
	it.Null = !a.Valid
}

type MaybeUInt64 struct {
	Value uint64
	Null  bool
}

func (it MaybeUInt64) ToSqlDriver() sql.NullInt64 {
	return sql.NullInt64{int64(it.Value), !it.Null}
}

func (it *MaybeUInt64) FromSqlDriver(a sql.NullInt64) {
	it.Value = uint64(a.Int64)
	it.Null = !a.Valid
}

type MaybeFloat32 struct {
	Value float32
	Null  bool
}

func (it MaybeFloat32) ToSqlDriver() sql.NullFloat64 {
	return sql.NullFloat64{float64(it.Value), !it.Null}
}

func (it *MaybeFloat32) FromSqlDriver(a sql.NullFloat64) {
	it.Value = float32(a.Float64)
	it.Null = !a.Valid
}

type MaybeFloat64 struct {
	Value float64
	Null  bool
}

func (it MaybeFloat64) ToSqlDriver() sql.NullFloat64 {
	return sql.NullFloat64{it.Value, !it.Null}
}

func (it *MaybeFloat64) FromSqlDriver(a sql.NullFloat64) {
	it.Value = a.Float64
	it.Null = !a.Valid
}

type MaybeString struct {
	Value string
	Null  bool
}

func (it MaybeString) ToSqlDriver() sql.NullString {
	return sql.NullString{it.Value, !it.Null}
}

func (it *MaybeString) FromSqlDriver(a sql.NullString) {
	it.Value = a.String
	it.Null = !a.Valid
}

type MaybeDecimal struct {
	Value decimal.Decimal
	Null  bool
}

func (it MaybeDecimal) ToSqlDriver() []byte {
	if it.Null {
		return nil
	}
	return []byte(it.Value.String())
}

func (it *MaybeDecimal) FromSqlDriver(a []byte) {
	if a == nil {
		it.Null = true
		return
	}
	var err error
	it.Value, err = decimal.NewFromString(string(a))
	it.Null = (err != nil)
}

func BigIntToBytes(a *big.Int) []byte {
	if a == nil {
		return nil
	}
	return []byte(a.String())
}

func BigIntFromBytes(a []byte) *big.Int {
	if a == nil {
		return nil
	}
	var ok bool
	var r *big.Int = &big.Int{}
	r, ok = r.SetString(string(a), 10)
	if ok {
		return r
	} else {
		return nil
	}
}

func DecimalToBytes(a decimal.Decimal) []byte {
	return []byte(a.String())
}

func DecimalFromBytes(a []byte) decimal.Decimal {
	var dec decimal.Decimal
	dec, _ = decimal.NewFromString(string(a))
	return dec
}
