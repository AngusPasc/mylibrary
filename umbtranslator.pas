unit UMBTranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
  TMBTranslator = Class
     private
        FTranslatorName : String;
        FTranslatorSurname : String;
        FTranslatorID : Integer;
        FNewTranslator : Boolean;
        FTranslatorCompositionsID : array of Integer;
        function GetTranslatorCompositionID(Index : Integer) : Integer;
        procedure GetTranslatorCompositions(const SQLQuery :  TSQLQuery);
        procedure SetTranslatorName( Name : String );
        procedure SetTranslatorSurname( Surname : String );
     public
        constructor Create(Name : String = ''; Surname : String = '');
        constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
        property TranslatorName : String read FTranslatorName write SetTranslatorName;
        property TranslatorSurname : String read FTranslatorSurname write SetTranslatorSurname;
        property TranslatorID : Integer read FTranslatorID;
        property TranslatorComposition[Index : Integer]: Integer read GetTranslatorCompositionID;
        procedure UpdateTranslator(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

     published
  end;

implementation

constructor TMBTranslator.Create(Name : String = ''; Surname : String = '');
begin
     //ДК, надо добавить пару проверок Name и Surname не должны быть пустыми
     FTranslatorName:=Name;
     FTranslatorSurname:=Surname;
     FTranslatorID:=0;
     FNewTranslator := True;
end;

constructor TMBTranslator.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, surname FROM translators where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FTranslatorName:=SQLQuery.FieldByName('name').AsString;
     FTranslatorSurname:=SQLQuery.FieldByName('surname').AsString;
     FTranslatorID:=ID;
     SQLQuery.Close;
     FNewTranslator := False;
end;

function TMBTranslator.GetTranslatorCompositionID(Index : Integer) : Integer;
begin
     if Index < Length(FTranslatorCompositionsID) then
        Result := FTranslatorCompositionsID[Index]
     else
         Result := -1;
end;

procedure TMBTranslator.GetTranslatorCompositions(const SQLQuery :  TSQLQuery);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT composition_id FROM rel_translator_compositions where translator_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FTranslatorID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FTranslatorCompositionsID, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FTranslatorCompositionsID[I]:=SQLQuery.FieldByName('composition_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

procedure TMBTranslator.SetTranslatorName( Name : String );
begin
     //ДК, нужна проверка на значение переменной Name
     FTranslatorName := Name;
end;

procedure TMBTranslator.SetTranslatorSurname( Surname : String );
begin
     FTranslatorSurname := Surname;
end;

procedure TMBTranslator.UpdateTranslator(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     if FNewTranslator = True then
     begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='inset into translator (name, surname) values (:bTranslatorName, :bTranslatorSurname )';
          SQLQuery.Params.ParamByName('bTranslatorName').AsString:=FTranslatorName;
          SQLQuery.Params.ParamByName('bTranslatorSurname').AsString:=FTranslatorSurname;
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
          FNewTranslator := False;
     end
     else
     begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='update translator set name=:bTranslatorName, surname=:bTranslatorSurname where id =:bTranslatorID';
          SQLQuery.Params.ParamByName('bTranslatorName').AsString:=FTranslatorName;
          SQLQuery.Params.ParamByName('bTranslatorSurname').AsString:=FTranslatorSurname;
          SQLQuery.Params.ParamByName('bTranslatorID').AsString:=IntToStr(FTranslatorID);
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
     end;
     SQLQuery.Close;
end;

end.

