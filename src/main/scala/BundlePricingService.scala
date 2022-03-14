

import BundlePricingDomain._

import scala.util.{Failure, Try}

/**
 * Specs for the BundlePricingService, which take as parameter a catalog and the current promotions
 * and then can bundle a cart to optimize the price
 */
class BundlePricingService(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {

  /**
   * Group cart item to bundles to get the lowest possible cart price
   * @return
   *   Success: cart price in cents, example Price(2250) => $22.50
   *   Failure: InvalidCartException if the cart isn't valid (contains an item which doesn't exist in catalog)
   */

  case class Applied(cart: Seq[CartItem], total: Price)  {
    def add(a2: Applied): Applied = {
      Applied(a2.cart, Price(a2.total.value + total.value))
    }
  }




  def bundleCartToLowestPrice(cart: Cart): Try[Price] = {
    isCartValid(cart) match {
      case true => Try {optimizePrice(cart)}
      case false => Failure{InvalidCartException}
    }
  }

  def computeAllCosts(cart: Cart): Seq[Price] = {
    val permutations = bundlePromotions.permutations
    permutations
      .map{promos =>
        val empty = Applied(cart.cartItems, Price(0))
        val total = promos.foldRight(empty){ (a, b) =>
          chainPromo(a, b)
        }
        total.cart.foldRight(total.total){(a, b) => Price(b.value+cartItemPrice(a).value)}
      }.toSeq
  }

  def optimizePrice(cart: Cart): Price = {
    computeAllCosts(cart).
      filter(_.value>0).minBy(_.value)
  }


  def cartItemPrice(item: CartItem): Price =
    Price(item.quantity.value*item.catalogItem.unitPrice.value)


  def chainPromo(promo: BundlePromotion, previousChain: Applied): Applied = {
    val currentApplied = promo match {
      case p: BundlePromotions.BundleDiscountOnItemUnitPrice => applyOnPrice(p, previousChain.cart)
      case p: BundlePromotions.BundleTotalPriceDiscount => applyTPD(p, previousChain.cart)
    }
    val newTotal = currentApplied.total.value + previousChain.total.value
    Applied(currentApplied.cart, Price(newTotal))
  }



  def applyTPD(p: BundlePromotions.BundleTotalPriceDiscount, cart: Seq[CartItem]): Applied = {

    val promoItem = p.cartItems.head // only n=1 for a total price discount
    def loop(curCart: Seq[CartItem], i: Int): Applied = curCart.toList match {
      case Nil => Applied(cart, Price(0))
      case cartHead :: _ if cartHead.catalogItem==promoItem.catalogItem =>
        if(cartHead.quantity.value>=promoItem.quantity.value) {
          val applyTimesX = cartHead.quantity.value/promoItem.quantity.value
          val leftOver = cartHead.quantity.value%promoItem.quantity.value

          val totaled = applyTimesX * p.totalDiscountedPrice.value
          if(leftOver>=1) {
            val newCart = cart.updated(i, CartItem(cartHead.catalogItem, Quantity(leftOver)))
            Applied(newCart, Price(totaled)).add(loop(newCart, i))
          }
          else {
            Applied(cart.filter(_!=cartHead), Price(totaled)) // deleted, inefficient cause linear search
          }
        }
        else {
          Applied(curCart, cartItemPrice(cartHead))
        }
      case _ :: t => loop(t, i+1)
    }

    loop(cart, 0)
  }

  def isPromoItem(p: BundlePromotions.BundleDiscountOnItemUnitPrice, item: CartItem): Boolean = {
    p.cartItems.foreach{promoItem =>
      if(item.catalogItem==promoItem.catalogItem) {
        return true
      }
    }
    false
  }

  def applyOnPrice(p: BundlePromotions.BundleDiscountOnItemUnitPrice, cart: Seq[CartItem]): Applied = cart.toList match {
    case Nil => Applied(cart, Price(0))
    case h :: _ if isPromoItem(p,h) => applyFoundOnUnit(p.cartItems, cart, p.totalDiscountedPrice)
    case _ :: t => applyOnPrice(p, t)
  }

  def foundItem(c: CartItem, promo: Seq[CartItem]): Boolean =
    promo.contains(c)





  def remove(item: CartItem, cart: Seq[CartItem]): Seq[CartItem] = {
    // remove as many as it needs...
    cart.foldRight(Seq[CartItem]()){(cartItem, foldingCart) =>
      if(cartItem.catalogItem==item.catalogItem) {
        val leftOver = cartItem.quantity.value - item.quantity.value
        if(leftOver>0) {
          foldingCart:+CartItem(item.catalogItem, Quantity(leftOver))
        }
        else {
          foldingCart
        }
      }
      else {
        foldingCart:+cartItem
      }
    }

  }

  def applyFoundOnUnit(p: Seq[CartItem], cart: Seq[CartItem], price: Price): Applied = p.toList match {
    case Nil => Applied(cart, price)
    case h :: t => applyFoundOnUnit(t, remove(h,cart), price)
  }


  def exists(item: CartItem)= catalog.contains(item.catalogItem)


  def isCartValid(cart: Cart): Boolean = {
    cart.cartItems.foreach{
      a => if(!exists(a)) return false
    }
    true
  }

}

