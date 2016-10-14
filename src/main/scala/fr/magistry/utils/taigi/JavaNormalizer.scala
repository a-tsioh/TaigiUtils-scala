package fr.magistry.utils.taigi

import java.text.{Normalizer => N}

import fr.magistry.nlp.scripts.Normalizer

object JavaNormalizer extends Normalizer {
  def normalizeNFKC(a: String) = N.normalize(a, N.Form.NFKC)
  def normalizeNFC(a: String) = N.normalize(a, N.Form.NFC)
  def normalizeNFD(a: String) = N.normalize(a, N.Form.NFD)

}