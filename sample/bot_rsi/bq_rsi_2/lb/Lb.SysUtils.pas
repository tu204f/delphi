unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  ///<summary>
  /// Свеча
  ///</summary>
  TCandel = record
    Time: Integer;  // Дата и время
    Open: Double;   // Цена открытие
    High: Double;   // Максимальная цена
    Low: Double;    // Минимальная цена
    Close: Double;  // Закрытие цены
    Vol: Double;    // Объем который прошол
  end;

  ///<summary>
  /// Массив свячей
  ///</summary>
  TCandelList = TList<TCandel>;

implementation

end.
