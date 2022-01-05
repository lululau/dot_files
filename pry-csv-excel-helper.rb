require 'csv'
require 'caxlsx'
require 'roo'
require 'active_support/all'

CSV_BOM = "\xef\xbb\xbf"

module Kernel
  def generate_csv(filename, **options, &block)
    opts = {
      col_sep: "\t",
      row_sep: "\r\n"
    }
    opts.merge!(options.except(:encoding))
    encoding = options[:encoding] || 'UTF-16LE'
    File.open(File.expand_path(filename), "w:#{encoding}") do |file|
      file.write(CSV_BOM)
      file.write CSV.generate(**opts, &block)
    end
  end

  def parse_csv(filename, **options)
    encoding = options[:encoding] || 'UTF-16'
    opts = {
      headers: false,
      col_sep: "\t",
      row_sep: "\r\n"
    }
    opts.merge!(options.except(:encoding))
    CSV.parse(IO.read(File.expand_path(filename), encoding: encoding, binmode: true).encode('UTF-8'), **opts).to_a
  end

  def generate_excel(filename)
    Axlsx::Package.new do |package|
      yield(package.workbook)
      package.serialize(filename)
    end
  end

  def parse_excel(filename)
    xlsx = Roo::Excelx.new(File.expand_path(filename))
    xlsx.sheets.each_with_object({}) do |sheet_name, result|
      begin
        result[sheet_name] = xlsx.sheet(sheet_name).to_a
      rescue
      end
    end
  end
end

class Array
  def write_csv(filename, *fields, **options)
    generate_csv(filename, **options) do |csv|
      if size > 0 && first.is_a?(ActiveRecord::Base)
        if fields.empty?
          fields = first.attributes.keys
        else
          fields = fields.map(&:to_s)
        end
        csv << fields
      end
      if size > 0 && first.is_a?(Hash)
        if fields.empty?
          fields = first.keys
        end
        csv << fields
      end
      each do |row|
        if row.is_a?(Array)
          csv << row.map(&:to_s)
        else
          csv << row.slice(*fields).values.map(&:to_s)
        end
      end
    end
  end

  def write_excel(filename, *fields, **options)
    sheet_name = options[:sheet_name] || 'Sheet1'
    generate_excel(filename) do |workbook|
      workbook.add_worksheet(name: sheet_name) do |sheet|
        if size > 0 && first.is_a?(ActiveRecord::Base)
          if fields.empty?
            fields = first.attributes.keys
          else
            fields = fields.map(&:to_s)
          end
          sheet.add_row(fields, types: [:string] * fields.size)
        end
        if size > 0 && first.is_a?(Hash)
          if fields.empty?
            fields = first.keys
          end
          sheet.add_row(fields, types: [:string] * fields.size)
        end
        each do |row|
          if row.is_a?(Array)
            sheet.add_row(row.map(&:to_s), types: [:string] * row.size)
          else
            sheet.add_row(row.slice(*fields).values.map(&:to_s), types: [:string] * fields.size)
          end
        end
      end
    end
  end
end

class Hash
  def write_excel(filename)
    generate_excel(filename) do |workbook|
      each do |sheet_name, sheet_data|
        workbook.add_worksheet(name: sheet_name) do |sheet|
          if sheet_data.is_a?(Hash)
            fields = sheet_data[:fields].map(&:to_s)
            sheet.add_row(fields, types: [:string] * fields.size)
            sheet_data[:data].each do |row|
              sheet.add_row(row.slice(*fields).values.map(&:to_s), types: [:string] * fields.size)
            end
          end

          if sheet_data.is_a?(Array)
            if sheet_data.size > 0 && sheet_data.first.is_a?(ActiveModel::Base)
              fields = sheet_data.first.attributes.keys
              sheet.add_row(fields, types: [:string] * fields.size)
              sheet_data.each do |row|
                sheet.add_row(row.slice(*fields).values.map(&:to_s), types: [:string] * fields.size)
              end
            end

            if sheet_data.size > 0 && sheet_data.first.is_a?(Hash)
              fields = sheet_data.first.keys
              sheet.add_row(fields, types: [:string] * fields.size)
              sheet_data.each do |row|
                sheet.add_row(row.slice(*fields).values.map(&:to_s), types: [:string] * fields.size)
              end
            end

            if sheet_data.size > 0 && sheet_data.first.is_a?(Array)
              sheet_data.each do |row|
                sheet.add_row(row.map(&:to_s), types: [:string] * fields.size)
              end
            end
          end
        end
      end
    end
  end
end
