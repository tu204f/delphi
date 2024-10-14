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
  /// �����
  ///</summary>
  TCandel = record
    Time: Integer;  // ���� � �����
    Open: Double;   // ���� ��������
    High: Double;   // ������������ ����
    Low: Double;    // ����������� ����
    Close: Double;  // �������� ����
    Vol: Double;    // ����� ������� ������
  end;

  ///<summary>
  /// ������ ������
  ///</summary>
  TCandelList = TList<TCandel>;

implementation

end.
