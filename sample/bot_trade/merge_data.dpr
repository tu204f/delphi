// Проект склейки файлов данный в один файл
program merge_data;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.SysUtils;

const
  //PATH = 'd:\work\git\delphi\sample\data\sber\';
  PATH = 'd:\work\git\delphi\sample\data\gazp\';

function GetFileName(const ANamePaper: String; const AYear: Integer): String;
var
  xS: String;
begin
  // SBER_100101_101231.csv
  // SBER_220101_220709.csv
  if AYear < 22 then
    xS := Format('%s_%d0101_%d1231.csv',[ANamePaper,AYear,AYear])
  else
    xS := Format('%s_%d0101_%d0709.csv',[ANamePaper,AYear,AYear]);
  Result := PATH + xS;
end;


procedure SetMerge(const ASource, ABuffer: TStrings);
begin
  ASource.Delete(0);
  for var S in ASource do
    ABuffer.Add(S);
end;


var
  localBuffer: TStrings;
begin
  try

    localBuffer := TStringList.Create;

    for var i := 10 to 22 do
    begin
      var xS := GetFileName('GAZP',i);
      Writeln('Path:',xS);


      var xStr := TStringList.Create;
      xStr.LoadFromFile(xS);


      SetMerge(xStr, localBuffer);

      Writeln(' >> Source.Count = ',xStr.Count,'; Buffer.Count = ',localBuffer.Count);

    end;

    localBuffer.SaveToFile(PATH + 'data.csv');

    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
